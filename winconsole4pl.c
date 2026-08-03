/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* Drive a Windows console and read its screen buffer back.
 *
 * This exists to test what swipl.exe draws on a real console.  Every
 * other way we have of testing the line editor puts some emulator of
 * our own on the other end, which can only ever show that libedit does
 * what we think it does.  Here the screen is conhost's own buffer,
 * read cell by cell with ReadConsoleOutputW(), so it shows what a user
 * would actually see.
 *
 * The console belongs to *this* process and the child merely inherits
 * it.  The alternative -- give the child its own console with
 * CREATE_NEW_CONSOLE and then AttachConsole() to it -- means detaching
 * from our own console first, and a process can only be attached to one
 * console at a time, so we would be handing our standard output back
 * and forth for every read.  Owning it outright costs nothing: a
 * process that has no console of its own is free to allocate one, and
 * our standard output is a pipe under ctest, unaffected either way.
 */

#include <windows.h>
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <string.h>
#include <stdlib.h>

		 /*******************************
		 *            HANDLE            *
		 *******************************/

/* A process has one console, so there is nothing to hand back and no
 * lifetime to manage: the state lives here.  Opening a second one is an
 * error rather than a second handle, which is also what the OS would
 * say.
 */

typedef struct
{ HANDLE	hIn;			/* CONIN$, shared with our console */
  HANDLE	hOut;			/* screen buffer of our own */
  HANDLE	hProcess;		/* the program, if any */
  DWORD		pid;
  DWORD		saved_in_mode;		/* input mode as we found it */
  bool		have_saved_mode;
  bool		allocated;		/* we called AllocConsole() */
} wincon;

static wincon console;


/* Give up the console and everything we hold on it. */

static void
free_wincon(wincon *c)
{ if ( c->hProcess )
  { TerminateProcess(c->hProcess, 0);
    CloseHandle(c->hProcess);
    c->hProcess = NULL;
  }
  if ( c->hIn && c->hIn != INVALID_HANDLE_VALUE )
  { CloseHandle(c->hIn);
    c->hIn = NULL;
  }
  if ( c->hOut && c->hOut != INVALID_HANDLE_VALUE )
  { CloseHandle(c->hOut);
    c->hOut = NULL;
  }
  if ( c->allocated )
  { FreeConsole();
    c->allocated = false;
  }
  c->pid = 0;
}


/* The console every predicate below works on, or an existence_error
 * when none has been opened.
 */

static bool
get_wincon(wincon **cp)
{ if ( console.hOut )
  { *cp = &console;
    return true;
  }

  *cp = NULL;
  { term_t t;

    return ( (t=PL_new_term_ref()) &&
	     PL_put_atom_chars(t, "win_console") &&
	     PL_existence_error("win_console", t) );
  }
}


		 /*******************************
		 *            ERRORS            *
		 *******************************/

/* Raise error(system_error(Code, Message), context(Pred, _)) for the
 * last Windows error.  Reporting the message rather than only the code
 * matters here: most of what can go wrong (no console, a handle that is
 * not a screen buffer, a size the console refuses) is diagnosed only by
 * what the API says about it.
 */

static bool
win_error_id(DWORD id, const char *pred)
{ LPWSTR msgw = NULL;
  DWORD len;
  term_t ex, msg;

  len = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		       FORMAT_MESSAGE_FROM_SYSTEM |
		       FORMAT_MESSAGE_IGNORE_INSERTS,
		       NULL, id, 0, (LPWSTR)&msgw, 0, NULL);
  while( len > 0 && (msgw[len-1] == L'\r' || msgw[len-1] == L'\n' ||
		     msgw[len-1] == L'.'  || msgw[len-1] == L' ') )
    len--;

  if ( (ex=PL_new_term_ref()) &&
       (msg=PL_new_term_ref()) &&
       PL_unify_wchars(msg, PL_ATOM, len, msgw ? msgw : L"") &&
       PL_unify_term(ex,
		     PL_FUNCTOR_CHARS, "error", 2,
		       PL_FUNCTOR_CHARS, "system_error", 2,
		         PL_INTPTR, (intptr_t)id,
		         PL_TERM, msg,
		       PL_FUNCTOR_CHARS, "context", 2,
		         PL_CHARS, pred,
		         PL_VARIABLE) )
    PL_raise_exception(ex);

  if ( msgw )
    LocalFree(msgw);

  return false;
}


static bool
win_error(const char *pred)
{ return win_error_id(GetLastError(), pred);
}


		 /*******************************
		 *            GEOMETRY          *
		 *******************************/

/* Make buffer and window both exactly cols x rows.
 *
 * Equal sizes are what makes the readback meaningful: with a buffer
 * taller than the window there is scrollback, row 0 of the window is
 * not row 0 of the buffer, and every coordinate has to be taken
 * relative to srWindow.  The console also refuses a buffer smaller than
 * its window, hence the shrink-window / set-buffer / grow-window dance
 * rather than two plain calls.
 */

static bool
set_console_size(HANDLE hOut, int cols, int rows, const char *pred)
{ SMALL_RECT minimal = { 0, 0, 1, 1 };
  COORD size;
  SMALL_RECT rect;

  if ( !SetConsoleWindowInfo(hOut, TRUE, &minimal) )
    return win_error(pred);

  size.X = (SHORT)cols;
  size.Y = (SHORT)rows;
  if ( !SetConsoleScreenBufferSize(hOut, size) )
    return win_error(pred);

  rect.Left   = 0;
  rect.Top    = 0;
  rect.Right  = (SHORT)(cols-1);
  rect.Bottom = (SHORT)(rows-1);
  if ( !SetConsoleWindowInfo(hOut, TRUE, &rect) )
    return win_error(pred);

  return true;
}


		 /*******************************
		 *             OPEN             *
		 *******************************/

static foreign_t
win_console_open(term_t A1, term_t A2)
{ int cols, rows;
  wincon *c = &console;
  SECURITY_ATTRIBUTES sa = { sizeof(sa), NULL, TRUE };
  DWORD mode, err;

  if ( !PL_get_integer_ex(A1, &cols) ||
       !PL_get_integer_ex(A2, &rows) )
    return false;
  if ( cols < 2 || rows < 2 )
    return PL_domain_error("console_size", A1);

  if ( c->hOut )
    return PL_permission_error("open", "win_console", A1);

  /* A console we can read without disturbing whoever else is using it.
     The program gets a screen buffer of our own, created here and never
     made the active one, so nothing it draws is visible and nothing the
     caller prints lands in the buffer we read back.  Taking over CONOUT$
     instead would mean the caller's own output going into the screen
     under test.

     Use the console we already have where we can, because allocating
     one replaces it and takes the caller's standard output with it.
     Being able to open CONIN$ on it is the test of that: a process with
     no console, or with one it cannot read keys from, gets its own. */
  c->hIn = CreateFileW(L"CONIN$", GENERIC_READ|GENERIC_WRITE,
		       FILE_SHARE_READ|FILE_SHARE_WRITE, &sa,
		       OPEN_EXISTING, 0, NULL);
  if ( c->hIn == INVALID_HANDLE_VALUE )
  { FreeConsole();			/* we may not have one; never mind */
    if ( !AllocConsole() )
      return win_error_id(GetLastError(), "win_console_open/2: AllocConsole");
    c->allocated = true;

    { HWND hwnd = GetConsoleWindow();	/* of no use to anyone */

      if ( hwnd )
	ShowWindow(hwnd, SW_HIDE);
    }

    c->hIn = CreateFileW(L"CONIN$", GENERIC_READ|GENERIC_WRITE,
			 FILE_SHARE_READ|FILE_SHARE_WRITE, &sa,
			 OPEN_EXISTING, 0, NULL);
    if ( c->hIn == INVALID_HANDLE_VALUE )
    { err = GetLastError();
      free_wincon(c);
      return win_error_id(err, "win_console_open/2: CONIN$");
    }
  }

  c->hOut = CreateConsoleScreenBuffer(GENERIC_READ|GENERIC_WRITE,
				      FILE_SHARE_READ|FILE_SHARE_WRITE,
				      &sa, CONSOLE_TEXTMODE_BUFFER, NULL);
  if ( c->hOut == INVALID_HANDLE_VALUE )
  { err = GetLastError();
    c->hOut = NULL;
    free_wincon(c);
    return win_error_id(err, "win_console_open/2: CreateConsoleScreenBuffer");
  }

  if ( GetConsoleMode(c->hIn, &c->saved_in_mode) )
    c->have_saved_mode = true;

  /* Quick-edit turns a stray mouse event into a selection, and a
     selection freezes everything the child writes.  Mouse and window
     events are noise we never read. */
  if ( GetConsoleMode(c->hIn, &mode) )
  { mode &= ~(ENABLE_QUICK_EDIT_MODE|ENABLE_MOUSE_INPUT);
    mode |= ENABLE_EXTENDED_FLAGS;
    SetConsoleMode(c->hIn, mode);
  }

  if ( !set_console_size(c->hOut, cols, rows, "win_console_open/2") )
  { free_wincon(c);
    return false;			/* exception already raised */
  }

  return true;
}


		 /*******************************
		 *            SPAWN             *
		 *******************************/

static foreign_t
win_console_spawn(term_t A1)
{ wincon *c;
  wchar_t *cmd;
  size_t len;
  STARTUPINFOW si;
  PROCESS_INFORMATION pi;
  wchar_t *cmdbuf;

  if ( !get_wincon(&c) )
    return false;
  if ( !PL_get_wchars(A1, &len, &cmd, CVT_ALL|CVT_EXCEPTION) )
    return false;

  /* CreateProcessW may modify the command line in place, so it may not
     be handed Prolog's copy. */
  if ( !(cmdbuf=malloc((len+1)*sizeof(wchar_t))) )
    return PL_resource_error("memory");
  wcscpy(cmdbuf, cmd);

  memset(&si, 0, sizeof(si));
  si.cb         = sizeof(si);
  si.dwFlags    = STARTF_USESTDHANDLES;
  si.hStdInput  = c->hIn;
  si.hStdOutput = c->hOut;
  si.hStdError  = c->hOut;

  /* No CREATE_NEW_CONSOLE: the child is to use ours, which is what
     makes its output land in the buffer we read back. */
  if ( !CreateProcessW(NULL, cmdbuf, NULL, NULL, TRUE,
		       0, NULL, NULL, &si, &pi) )
  { free(cmdbuf);
    return win_error("win_console_spawn/1");
  }
  free(cmdbuf);

  CloseHandle(pi.hThread);
  if ( c->hProcess )
    CloseHandle(c->hProcess);
  c->hProcess = pi.hProcess;
  c->pid      = pi.dwProcessId;

  return true;
}


static foreign_t
win_console_wait(term_t A1, term_t A2)
{ wincon *c;
  int timeout;
  DWORD rc, status;

  if ( !get_wincon(&c) ||
       !PL_get_integer_ex(A1, &timeout) )
    return false;
  if ( !c->hProcess )
    return PL_existence_error("process", A1);

  rc = WaitForSingleObject(c->hProcess, timeout < 0 ? INFINITE
						    : (DWORD)timeout);
  if ( rc == WAIT_TIMEOUT )
    return false;
  if ( rc != WAIT_OBJECT_0 )
    return win_error("win_console_wait/2");
  if ( !GetExitCodeProcess(c->hProcess, &status) )
    return win_error("win_console_wait/2");

  return PL_unify_integer(A2, (int)status);
}


static foreign_t
win_console_kill(void)
{ wincon *c;

  if ( !get_wincon(&c) )
    return false;
  if ( c->hProcess )
    TerminateProcess(c->hProcess, 1);

  return true;
}


		 /*******************************
		 *             INPUT            *
		 *******************************/

/* Append a key-down and key-up record for one event. */

static bool
add_key(INPUT_RECORD *ir, size_t *np, WORD vk, wchar_t chr, DWORD ctrl)
{ size_t n = *np;
  int i;

  for(i=0; i<2; i++)
  { ir[n].EventType = KEY_EVENT;
    ir[n].Event.KeyEvent.bKeyDown          = (i == 0);
    ir[n].Event.KeyEvent.wRepeatCount      = 1;
    ir[n].Event.KeyEvent.wVirtualKeyCode   = vk;
    ir[n].Event.KeyEvent.wVirtualScanCode  = 0;
    ir[n].Event.KeyEvent.uChar.UnicodeChar = chr;
    ir[n].Event.KeyEvent.dwControlKeyState = ctrl;
    n++;
  }

  *np = n;
  return true;
}


static bool
send_records(wincon *c, INPUT_RECORD *ir, size_t n, const char *pred)
{ size_t done = 0;

  while(done < n)
  { DWORD written = 0;

    if ( !WriteConsoleInputW(c->hIn, ir+done, (DWORD)(n-done), &written) )
      return win_error(pred);
    if ( written == 0 )
      break;
    done += written;
  }

  return true;
}


/* win_console_send(+Console, +Text)
 *
 * Type Text.  Each UTF-16 unit becomes a key event carrying it as its
 * character; a surrogate pair is two events, which is how a console
 * reports one anyway.  Control characters are sent as themselves: the
 * point is to reach the reader, not to reproduce which physical key
 * would have produced them.
 */

static foreign_t
win_console_send(term_t A1)
{ wincon *c;
  wchar_t *s;
  size_t len;
  INPUT_RECORD *ir;
  size_t n = 0, i;
  bool rc;

  if ( !get_wincon(&c) ||
       !PL_get_wchars(A1, &len, &s, CVT_ALL|CVT_EXCEPTION) )
    return false;
  if ( len == 0 )
    return true;

  if ( !(ir=calloc(2*len, sizeof(*ir))) )
    return PL_resource_error("memory");

  for(i=0; i<len; i++)
    add_key(ir, &n, 0, s[i], 0);

  rc = send_records(c, ir, n, "win_console_send/1");
  free(ir);

  return rc;
}


/* win_console_key(+Console, +Key)
 *
 * Press a named key.  These go as virtual key codes with no character,
 * which is what a console reports for them; conhost turns them into the
 * escape sequence the reader expects.  Sending that sequence directly
 * would skip the very translation this is here to test.
 */

static const struct
{ const char *name;
  WORD vk;
  wchar_t chr;
} named_keys[] =
{ { "cursor_up",    VK_UP,     0 },
  { "cursor_down",  VK_DOWN,   0 },
  { "cursor_left",  VK_LEFT,   0 },
  { "cursor_right", VK_RIGHT,  0 },
  { "home",         VK_HOME,   0 },
  { "end",          VK_END,    0 },
  { "delete",       VK_DELETE, 0 },
  { "backspace",    VK_BACK,   L'\b' },
  { "enter",        VK_RETURN, L'\r' },
  { "tab",          VK_TAB,    L'\t' },
  { NULL,           0,         0 }
};


static foreign_t
win_console_key(term_t A1)
{ wincon *c;
  char *name;
  int i;
  INPUT_RECORD ir[2];
  size_t n = 0;

  if ( !get_wincon(&c) ||
       !PL_get_chars(A1, &name, CVT_ATOM|CVT_EXCEPTION) )
    return false;

  for(i=0; named_keys[i].name; i++)
  { if ( strcmp(named_keys[i].name, name) == 0 )
    { add_key(ir, &n, named_keys[i].vk, named_keys[i].chr, 0);
      return send_records(c, ir, n, "win_console_key/1");
    }
  }

  return PL_domain_error("win_console_key", A1);
}


		 /*******************************
		 *            SCREEN            *
		 *******************************/

/* win_console_mode(-InMode, -OutMode)
 *
 * The console's input and output modes.  Whether the output mode has
 * ENABLE_VIRTUAL_TERMINAL_PROCESSING (0x0004) decides whether the
 * escape sequences a program writes reach the screen buffer as
 * cursor motion or as text, so it is the first thing to look at when
 * the screen reads back full of escapes.
 */

static foreign_t
win_console_mode(term_t A1, term_t A2)
{ wincon *c;
  DWORD in = 0, out = 0;

  if ( !get_wincon(&c) )
    return false;
  if ( !GetConsoleMode(c->hIn, &in) || !GetConsoleMode(c->hOut, &out) )
    return win_error("win_console_mode/2");

  return ( PL_unify_integer(A1, (int)in) &&
	   PL_unify_integer(A2, (int)out) );
}


static foreign_t
win_console_size(term_t A1, term_t A2)
{ wincon *c;
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if ( !get_wincon(&c) )
    return false;
  if ( !GetConsoleScreenBufferInfo(c->hOut, &csbi) )
    return win_error("win_console_size/2");

  return ( PL_unify_integer(A1, csbi.srWindow.Right - csbi.srWindow.Left + 1) &&
	   PL_unify_integer(A2, csbi.srWindow.Bottom - csbi.srWindow.Top + 1) );
}


static foreign_t
win_console_resize(term_t A1, term_t A2)
{ wincon *c;
  int cols, rows;

  if ( !get_wincon(&c) ||
       !PL_get_integer_ex(A1, &cols) ||
       !PL_get_integer_ex(A2, &rows) )
    return false;

  return set_console_size(c->hOut, cols, rows, "win_console_resize/2");
}


static foreign_t
win_console_cursor(term_t A1, term_t A2)
{ wincon *c;
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if ( !get_wincon(&c) )
    return false;
  if ( !GetConsoleScreenBufferInfo(c->hOut, &csbi) )
    return win_error("win_console_cursor/2");

  return ( PL_unify_integer(A1, csbi.dwCursorPosition.X - csbi.srWindow.Left) &&
	   PL_unify_integer(A2, csbi.dwCursorPosition.Y - csbi.srWindow.Top) );
}


/* win_console_row(+Console, +Row, -Text)
 *
 * Text of one visible row, with trailing blanks removed.  The console
 * pads every row to the full width; the callers compare against what a
 * line was written as, so the padding is not content.
 *
 * A cell holds one UTF-16 unit.  The right-hand cell of a double-width
 * character repeats it with COMMON_LVB_TRAILING_BYTE set and is
 * skipped, the way the same character occupies one column in the text
 * it came from.  A surrogate pair spans two cells and is put back
 * together.
 *
 * Note what a cell cannot hold: a base and its combining marks.  The
 * console keeps one unit per cell, so text in NFD reaches the screen
 * buffer already reduced, and no reading of it can recover what was
 * written.  That is a property of the console, not of this code.
 */

static foreign_t
win_console_row(term_t A1, term_t A2)
{ wincon *c;
  int row;
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  CHAR_INFO *cbuf;
  COORD bufsize, bufcoord;
  SMALL_RECT region;
  int cols, i, len;
  wchar_t *text;
  bool rc;

  if ( !get_wincon(&c) ||
       !PL_get_integer_ex(A1, &row) )
    return false;
  if ( !GetConsoleScreenBufferInfo(c->hOut, &csbi) )
    return win_error("win_console_row/2");

  cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
  if ( row < 0 || row > csbi.srWindow.Bottom - csbi.srWindow.Top )
    return PL_unify_atom_chars(A2, "");

  if ( !(cbuf=calloc(cols, sizeof(*cbuf))) )
    return PL_resource_error("memory");
  if ( !(text=calloc(cols+1, sizeof(*text))) )
  { free(cbuf);
    return PL_resource_error("memory");
  }

  bufsize.X  = (SHORT)cols;
  bufsize.Y  = 1;
  bufcoord.X = 0;
  bufcoord.Y = 0;
  region.Left   = csbi.srWindow.Left;
  region.Right  = csbi.srWindow.Right;
  region.Top    = (SHORT)(csbi.srWindow.Top + row);
  region.Bottom = region.Top;

  if ( !ReadConsoleOutputW(c->hOut, cbuf, bufsize, bufcoord, &region) )
  { free(cbuf);
    free(text);
    return win_error("win_console_row/2");
  }

  for(i=0, len=0; i<cols; i++)
  { wchar_t ch = cbuf[i].Char.UnicodeChar;

    if ( (cbuf[i].Attributes & COMMON_LVB_TRAILING_BYTE) )
    { /* The right half of a double-width character.  Usually it just
	 repeats the character and is dropped, so that the character
	 reads as the one character it was written as.  But a character
	 outside the BMP is two UTF-16 units and the console keeps them
	 in the two cells, so a low surrogate here belongs to the high
	 surrogate we just took and completes it. */
      if ( len > 0 &&
	   text[len-1] >= 0xD800 && text[len-1] <= 0xDBFF &&
	   ch >= 0xDC00 && ch <= 0xDFFF )
	text[len++] = ch;
      continue;
    }
    text[len++] = ch;
  }
  while( len > 0 && text[len-1] == L' ' )
    len--;
  text[len] = 0;

  rc = PL_unify_wchars(A2, PL_ATOM, len, text);

  free(cbuf);
  free(text);

  return rc;
}


		 /*******************************
		 *            CLOSE             *
		 *******************************/

static foreign_t
win_console_close(void)
{ wincon *c;

  if ( !get_wincon(&c) )
    return false;

  free_wincon(c);			/* the blob itself goes with the GC */

  return true;
}


		 /*******************************
		 *           REGISTER           *
		 *******************************/

install_t
install_winconsole4pl(void)
{ PL_register_foreign("win_console_open",   2, win_console_open,   0);
  PL_register_foreign("win_console_spawn",  1, win_console_spawn,  0);
  PL_register_foreign("win_console_wait",   2, win_console_wait,   0);
  PL_register_foreign("win_console_kill",   0, win_console_kill,   0);
  PL_register_foreign("win_console_send",   1, win_console_send,   0);
  PL_register_foreign("win_console_key",    1, win_console_key,    0);
  PL_register_foreign("win_console_mode",   2, win_console_mode,   0);
  PL_register_foreign("win_console_size",   2, win_console_size,   0);
  PL_register_foreign("win_console_resize", 2, win_console_resize, 0);
  PL_register_foreign("win_console_cursor", 2, win_console_cursor, 0);
  PL_register_foreign("win_console_row",    2, win_console_row,    0);
  PL_register_foreign("win_console_close",  0, win_console_close,  0);
}

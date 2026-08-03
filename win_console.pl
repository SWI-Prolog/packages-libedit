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

:- module(win_console,
          [ win_console_open/2,         % +Cols, +Rows
            win_console_spawn/1,        % +CommandLine
            win_console_wait/2,         % +TimeOutMs, -Status
            win_console_kill/0,
            win_console_send/1,         % +Text
            win_console_key/1,          % +Key
            win_console_mode/2,         % -InMode, -OutMode
            win_console_size/2,         % -Cols, -Rows
            win_console_resize/2,       % +Cols, +Rows
            win_console_cursor/2,       % -Col, -Row
            win_console_row/2,          % +Row, -Text
            win_console_close/0
          ]).

/** <module> Drive a Windows console and read its screen back

Runs a program on a Windows console and reports what is on that
console, cell by cell, straight out of the screen buffer with
`ReadConsoleOutputW()`.

This exists so that what `swipl.exe` draws on a console can be checked
against the console itself rather than against an emulator of our own.
It is what packages/xpce/tests/test_terminal.pl uses for its `console`
backend; on other platforms that suite drives a terminal we wrote, which
can only ever show that the line editor does what we think it does.

The program is given a screen buffer of its own, created by
win_console_open/2 and never made the active one. Nothing it draws is
visible, and nothing the caller prints lands in the buffer being read
back, so this can be run straight from a toplevel on the very console
the caller is sitting on.

The console's *input* buffer is shared -- there is only one -- so keys
sent with win_console_send/1 go to whoever reads first. That is the
program under test while it runs, since the caller is busy running this.
win_console_close/0 puts the input mode back as it found it and drops
anything the program never read.

What a console cannot represent is a limit on what can be asserted about
it. A cell holds one UTF-16 unit, so a base character and its combining
marks cannot both survive: text in NFD is already reduced by the time it
reaches the screen buffer, and no reading of it can recover what was
written. Double-width characters occupy two cells, of which the second
repeats the character with `COMMON_LVB_TRAILING_BYTE` set;
win_console_row/2 drops those, so a wide character reads as the one
character it was written as.
*/

:- use_foreign_library(foreign(winconsole4pl)).

%!  win_console_open(+Cols, +Rows) is det.
%
%   Create a screen buffer of exactly Cols by Rows for the program to
%   draw on. A process has one console, so there is no handle: every
%   predicate below works on it, and opening a second raises a
%   permission_error. Buffer and window are made the same size, so there
%   is no scrollback and row 0 of the window is row 0 of the buffer.
%
%   A process with no console of its own gets one allocated, with its
%   window hidden.

%!  win_console_spawn(+CommandLine) is det.
%
%   Start CommandLine on Console. The program is given the console's
%   handles as its standard input, output and error, so it sees a
%   console rather than a pipe and takes the code paths a user would.

%!  win_console_wait(+TimeOutMs, -Status) is semidet.
%
%   Wait up to TimeOutMs for the program to exit and report its exit
%   status. A negative TimeOutMs waits forever. Fails on time-out.

%!  win_console_kill is det.
%
%   Terminate the program without waiting for it.

%!  win_console_send(+Text) is det.
%
%   Type Text. Control characters are sent as themselves: the point is
%   to reach the reader, not to reproduce which key would have produced
%   them.

%!  win_console_key(+Key) is det.
%
%   Press a named key: `cursor_up`, `cursor_down`, `cursor_left`,
%   `cursor_right`, `home`, `end`, `delete`, `backspace`, `enter` or
%   `tab`. These go as virtual key codes rather than as the escape
%   sequence they stand for, because turning the one into the other is
%   the console's job and part of what is under test.

%!  win_console_mode(-InMode, -OutMode) is det.
%
%   The console's input and output mode words. Whether OutMode has
%   `ENABLE_VIRTUAL_TERMINAL_PROCESSING` (0x0004) decides whether the
%   escape sequences a program writes reach the screen buffer as cursor
%   motion or as text, which is the first thing to look at when a row
%   reads back full of escapes.

%!  win_console_size(-Cols, -Rows) is det.
%!  win_console_resize(+Cols, +Rows) is det.
%
%   Size of the console in character cells. Resizing raises the window
%   size event a program watches for.

%!  win_console_cursor(-Col, -Row) is det.
%
%   Cursor position, 0-based from the top left of the window.

%!  win_console_row(+Row, -Text) is det.
%
%   Text of one visible row with trailing blanks removed. Rows outside
%   the window read as ''.

%!  win_console_close is det.
%
%   Terminate the program, give up the console and close its handles.

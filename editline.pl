/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2025, VU University Amsterdam
                              CWI Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(editline,
          [ el_wrap/0,                          % wrap user_input, etc.
            el_wrap/1,                          % +Options
            el_wrap/4,                          % +Prog, +Input, +Output, +Error
            el_wrap/5,                          % +Prog, +Input, +Output, +Error, +Options
            el_wrapped/1,                       % +Input
            el_unwrap/1,                        % +Input

            el_source/2,                        % +Input, +File
            el_bind/2,                          % +Input, +Args
            el_set/2,                           % +Input, +Action
            el_addfn/4,                         % +Input, +Name, +Help, :Goal
            el_cursor/2,                        % +Input, +Move
            el_line/2,                          % +Input, -Line
            el_insertstr/2,                     % +Input, +Text
            el_deletestr/2,                     % +Input, +Count

            el_history/2,                       % +Input, ?Action
            el_history_events/2,                % +Input, -Events
            el_add_history/2,                   % +Input, +Line
            el_write_history/2,                 % +Input, +FileName
            el_read_history/2,                  % +Input, +FileName

	    el_version/1			% -Version:integer
          ]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(lists),[reverse/2,max_list/2,append/3,member/2]).
:- autoload(library(solution_sequences),[call_nth/2]).
:- autoload(library(option), [merge_options/3]).

:- use_foreign_library(foreign(libedit4pl)).

:- initialization el_wrap_if_ok.

:- meta_predicate
    el_addfn(+,+,+,3).

:- multifile
    el_setup/1,                         % +Input
    prolog:complete_input/4.


/** <module> BSD libedit based command line editing

This library wraps the BSD  libedit   command  line  editor. The binding
provides a high level API to enable   command line editing on the Prolog
user streams and low level predicates  to   apply  the  library on other
streams and program the library.
*/

el_wrap_if_ok :-
    \+ current_prolog_flag(readline, readline),
    stream_property(user_input, tty(true)),
    !,
    el_wrap.
el_wrap_if_ok.

%!  el_wrap is det.
%!  el_wrap(+Options) is det.
%
%   Enable using editline on the standard   user streams if `user_input`
%   is connected to a terminal. This is   the  high level predicate used
%   for most purposes. The remainder of the library interface deals with
%   low level predicates  that  allows   for  applying  and  programming
%   libedit in non-standard situations.
%
%   The library is registered  with  _ProgName_   set  to  `swipl`  (see
%   el_wrap/4).
%
%   Options processed:
%
%     - pipes(+Boolean)
%       Used by Epilog windows to indicate we are reading from a Windows
%       named pipe in _overlapped_ mode.  Ignored on other platforms.
%     - history(+Size)
%       Size of the history.  Default is defined by the Prolog flag
%       `history` or `100` if this flag is not defined.
%     - alert_signo(+Integer)
%       Signal used for making thread_signal/2 work while the thread
%       is in a blocking system call.

el_wrap :-
    el_wrap([]).

el_wrap(_) :-
    el_wrapped(user_input),
    !.
el_wrap(Options) :-
    stream_property(user_input, tty(true)), !,
    findall(Opt, el_default(Opt), Defaults),
    merge_options(Options, Defaults, Options1),
    el_wrap(swipl, user_input, user_output, user_error, Options1),
    add_prolog_commands(user_input),
    ignore(el_set(user_input, wordchars("_"))),
    forall(el_setup(user_input), true).
el_wrap(_).

el_default(history(Size)) :-
    current_prolog_flag(history, Value),
    (   integer(Value),
        Value >= 0
    ->  Size = Value
    ;   Value == false
    ->  Size = 0
    ).
:- if(current_predicate(prolog_alert_signal/2)).
el_default(alert_signo(SigNo)) :-
    prolog_alert_signal(SigName, SigName),
    current_signal(SigName, SigNo, _Handler).
:- endif.

add_prolog_commands(Input) :-
    el_addfn(Input, complete, 'Complete atoms and files', complete),
    el_addfn(Input, show_completions, 'List completions', show_completions),
    el_addfn(Input, electric, 'Indicate matching bracket', electric),
    el_addfn(Input, isearch_history, 'Incremental search in history',
             isearch_history),
    el_bind(Input, ["^I",  complete]),
    el_bind(Input, ["^[?", show_completions]),
    el_bind(Input, ["^R",  isearch_history]),
    bind_electric(Input),
    add_paste_quoted(Input),
    el_source(Input, _).

%!  el_wrap(+ProgName:atom, +In:stream, +Out:stream, +Error:stream) is det.
%!  el_wrap(+ProgName:atom, +In:stream, +Out:stream, +Error:stream, +Options) is det.
%
%   Enable editline on  the  stream-triple   <In,Out,Error>.  From  this
%   moment on In is a handle to the command line editor.  Options:
%
%     - pipes(true)
%       Windows only. Assume the I/O is using pipes rather than a
%       console.  This is used for the Epilog terminal.
%
%   @arg ProgName is the name of the invoking program, used when reading
%   the editrc(5) file to determine which settings to use.

el_wrap(ProgName, In, Out, Error) :-
    el_wrap(ProgName, In, Out, Error, []).

%!  el_setup(+In:stream) is nondet.
%
%   This hooks is called as   forall(el_setup(Input),  true) _after_ the
%   input stream has been wrapped, the default Prolog commands have been
%   added and the  default  user  setup   file  has  been  sourced using
%   el_source/2. It can be used to define and bind additional commands.

%!  el_wrapped(+In:stream) is semidet.
%
%   True if In is a stream wrapped by el_wrap/3.

%!  el_unwrap(+In:stream) is det.
%
%   Remove the libedit wrapper for In and   the related output and error
%   streams.
%
%   @bug The wrapper creates =|FILE*|= handles that cannot be closed and
%   thus wrapping and unwrapping implies a (modest) memory leak.

%!  el_source(+In:stream, +File) is det.
%
%   Initialise editline by reading the contents of File.  If File is
%   unbound try =|$HOME/.editrc|=


%!  el_bind(+In:stream, +Args) is det.
%
%   Invoke the libedit `bind` command  with   the  given  arguments. The
%   example below lists the current key bindings.
%
%   ```
%   ?- el_bind(user_input, ['-a']).
%   ```
%
%   The predicate el_bind/2 is typically used   to bind commands defined
%   using el_addfn/4. Note that the C proxy   function has only the last
%   character of the command as context to find the Prolog binding. This
%   implies we cannot both bind  e.g.,  "^[?"   _and_  "?"  to  a Prolog
%   function.
%
%   @see editrc(5) for more information.

%!  el_addfn(+Input:stream, +Command, +Help, :Goal) is det.
%
%   Add a new command to the command  line editor associated with Input.
%   Command is the name of the command,  Help is the help string printed
%   with e.g. =|bind -a|= (see el_bind/2)  and   Goal  is  called of the
%   associated key-binding is activated.  Goal is called as
%
%       call(:Goal, +Input, +Char, -Continue)
%
%   where Input is the input stream providing access to the editor, Char
%   the activating character and Continue must   be instantated with one
%   of the known continuation  codes  as   defined  by  libedit: `norm`,
%   `newline`, `eof`, `arghack`, `refresh`,   `refresh_beep`,  `cursor`,
%   `redisplay`, `error` or `fatal`. In addition, the following Continue
%   code is provided.
%
%     * electric(Move, TimeOut, Continue)
%     Show _electric caret_ at Move positions to the left of the normal
%     cursor positions for the given TimeOut.  Continue as defined by
%     the Continue value.
%
%   The registered Goal typically used el_line/2 to fetch the input line
%   and el_cursor/2, el_insertstr/2 and/or  el_deletestr/2 to manipulate
%   the input line.
%
%   Normally el_bind/2 is used to associate   the defined command with a
%   keyboard sequence.
%
%   @see el_set(3) =EL_ADDFN= for details.

%!  el_set(+Input:stream, +Action) is semidet.
%
%   Interface to el_set() and el_wset().   Currently provided values for
%   Action are:
%
%     - wordchars(+Text)
%       Set the characters considered part of a _word_.  This feature
%       depends on el_wsey() ``EL_WORDCHARS``, which is only provided
%       in some recent versions of `libedit`.
%
%   This predicate fails silently of Action  is not implemented. Illegal
%   input raises in an exception.

%!  el_line(+Input:stream, -Line) is det.
%
%   Fetch the currently buffered input line. Line is a term line(Before,
%   After), where `Before` is  a  string   holding  the  text before the
%   cursor and `After` is a string holding the text after the cursor.

%!  el_cursor(+Input:stream, +Move:integer) is det.
%
%   Move the cursor Move  character   forwards  (positive)  or backwards
%   (negative).

%!  el_insertstr(+Input:stream, +Text) is det.
%
%   Insert Text at the cursor.

%!  el_deletestr(+Input:stream, +Count) is det.
%
%   Delete Count characters before the cursor.

%!  el_history(+In:stream, ?Action) is det.
%
%   Perform a generic action on the history. This provides an incomplete
%   interface to history() from libedit.  Supported actions are:
%
%     - clear
%       Clear the history.
%     - setsize(+Integer)
%       Set size of history to size elements.
%     - getsize(-Integer)
%       Unify Integer with the maximum size of the history.  Note that
%       this is _not_ the same as el_history() using ``H_GETSIZE``,
%       which returns the number of currently saved events. The number
%       of saved events may be computed from `first` and `last` or
%       using el_history_events/2.
%     - setunique(+Boolean)
%       Set flag that adjacent identical event strings should not be
%       entered into the history.
%     - first(-Num, -String)
%     - last(-Num, -String)
%     - curr(-Num, -String)
%     - prev(-Num, -String)
%     - next(-Num, -String)
%       Retrieve an event.  Num is the event number and String is the
%       event string.  Note that `first` is the most recent event and
%       `last` the oldest.
%     - set(Num)
%       Set the notion of _current_ to Num.
%     - prev_str(+Search, -Num, -String)
%     - next_str(+Search, -Num, -String)
%       Retrieve the previous or next event whose String starts with
%       Search.
%     - event(+Num, -String)
%       True when String represents event Num.   This is an extension to
%       the history() API, retrieving a numbered event without changing
%       the current notion.

%!  el_history_events(+In:stream, -Events:list(pair)) is det.
%
%   Unify Events with a list of pairs   of  the form `Num-String`, where
%   `Num` is the event number  and   `String`  is  the associated string
%   without terminating newline.

%!  el_add_history(+In:stream, +Line:text) is det.
%
%   Add a line to the command line history.

%!  el_read_history(+In:stream, +File:file) is det.
%
%   Read the history saved using el_write_history/2.
%
%   @arg File is a file specification for absolute_file_name/3.

%!  el_write_history(+In:stream, +File:file) is det.
%
%   Save editline history to File.  The   history  may be reloaded using
%   el_read_history/2.
%
%   @arg File is a file specification for absolute_file_name/3.

%!  el_version(-Version)
%
%   True when Version  is ``LIBEDIT_MAJOR*10000 + LIBEDIT_MINOR*100``.
%   The  version is  generated from  the include  file ``histedit.h``,
%   which implies that the actual version of the shared library may be
%   different.

%!  prolog:history(+Input, ?Action) is semidet.
%
%   Provide  the  plugable  interface  into   the  system  command  line
%   management.

:- multifile
    prolog:history/2.

prolog:history(Input, enabled) :-
    !,
    el_wrapped(Input),
    el_history(Input, getsize(Size)),
    Size > 0.
prolog:history(Input, add(Line)) :-
    !,
    el_add_history(Input, Line).
prolog:history(Input, load(File)) :-
    !,
    compat_read_history(Input, File).
prolog:history(Input, save(File)) :-
    !,
    el_write_history(Input, File).
prolog:history(Input, events(Events)) :-
    !,
    el_history_events(Input, Events).
prolog:history(Input, Command) :-
    public_command(Command),
    !,
    el_history(Input, Command).

public_command(first(_Num, _String)).
public_command(curr(_Num, _String)).
public_command(event(_Num, _String)).
public_command(prev_str(_Search, _Num, _String)).
public_command(clear).

%!  compat_read_history(+Input, +File) is det.
%
%   Read the saved history. This loads both  the LibEdit and old history
%   format used by `swipl-win.exe` before migrating to SDL.

compat_read_history(Input, File) :-
    catch(el_read_history(Input, File), error(editline(_),_), fail),
    !.
compat_read_history(Input, File) :-
    access_file(File, read),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_old_history(Input, In),
        close(In)),
    !.
compat_read_history(_, _).

read_old_history(Input, From) :-
    catch('$raw_read'(From, Line), error(_,_), fail),
    (   Line == end_of_file
    ->  true
    ;   string_concat(Line, '.', Event),
        el_add_history(Input, Event),
        read_old_history(Input, From)
    ).

		 /*******************************
		 *        ELECTRIC CARET	*
		 *******************************/

%!  bind_electric(+Input) is det.
%
%   Bind known close statements for electric input

bind_electric(Input) :-
    forall(bracket(_Open, Close), bind_code(Input, Close, electric)),
    forall(quote(Close), bind_code(Input, Close, electric)).

bind_code(Input, Code, Command) :-
    string_codes(Key, [Code]),
    el_bind(Input, [Key, Command]).


%!  electric(+Input, +Char, -Continue) is det.

electric(Input, Char, Continue) :-
    string_codes(Str, [Char]),
    el_insertstr(Input, Str),
    el_line(Input, line(Before, _)),
    (   string_codes(Before, Codes),
        nesting(Codes, 0, Nesting),
        reverse(Nesting, [Close|RevNesting])
    ->  (   Close = open(_,_)                   % open quote
        ->  Continue = refresh
        ;   matching_open(RevNesting, Close, _, Index)
        ->  string_length(Before, Len),         % Proper match
            Move is Index-Len,
            Continue = electric(Move, 500, refresh)
        ;   Continue = refresh_beep             % Not properly nested
        )
    ;   Continue = refresh_beep
    ).

matching_open_index(String, Index) :-
    string_codes(String, Codes),
    nesting(Codes, 0, Nesting),
    reverse(Nesting, [Close|RevNesting]),
    matching_open(RevNesting, Close, _, Index).

matching_open([Open|Rest], Close, Rest, Index) :-
    Open = open(Index,_),
    match(Open, Close),
    !.
matching_open([Close1|Rest1], Close, Rest, Index) :-
    Close1 = close(_,_),
    matching_open(Rest1, Close1, Rest2, _),
    matching_open(Rest2, Close, Rest, Index).

match(open(_,Open),close(_,Close)) :-
    (   bracket(Open, Close)
    ->  true
    ;   Open == Close,
        quote(Open)
    ).

bracket(0'(, 0')).
bracket(0'[, 0']).
bracket(0'{, 0'}).

quote(0'\').
quote(0'\").
quote(0'\`).

nesting([], _, []).
nesting([H|T], I, Nesting) :-
    (   bracket(H, _Close)
    ->  Nesting = [open(I,H)|Nest]
    ;   bracket(_Open, H)
    ->  Nesting = [close(I,H)|Nest]
    ),
    !,
    I2 is I+1,
    nesting(T, I2, Nest).
nesting([0'0, 0'\'|T], I, Nesting) :-
    !,
    phrase(skip_code, T, T1),
    difflist_length(T, T1, Len),
    I2 is I+Len+2,
    nesting(T1, I2, Nesting).
nesting([H|T], I, Nesting) :-
    quote(H),
    !,
    (   phrase(skip_quoted(H), T, T1)
    ->  difflist_length(T, T1, Len),
        I2 is I+Len+1,
        Nesting = [open(I,H),close(I2,H)|Nest],
        nesting(T1, I2, Nest)
    ;   Nesting = [open(I,H)]                   % Open quote
    ).
nesting([_|T], I, Nesting) :-
    I2 is I+1,
    nesting(T, I2, Nesting).

difflist_length(List, Tail, Len) :-
    difflist_length(List, Tail, 0, Len).

difflist_length(List, Tail, Len0, Len) :-
    List == Tail,
    !,
    Len = Len0.
difflist_length([_|List], Tail, Len0, Len) :-
    Len1 is Len0+1,
    difflist_length(List, Tail, Len1, Len).

skip_quoted(H) -->
    [H],
    !.
skip_quoted(H) -->
    "\\", [H],
    !,
    skip_quoted(H).
skip_quoted(H) -->
    [_],
    skip_quoted(H).

skip_code -->
    "\\", [_],
    !.
skip_code -->
    [_].


		 /*******************************
		 *           COMPLETION		*
		 *******************************/

%!  complete(+Input, +Char, -Continue) is det.
%
%   Implementation of the registered `complete`   editline function. The
%   predicate is called with three arguments,  the first being the input
%   stream used to access  the  libedit   functions  and  the second the
%   activating character. The last argument tells   libedit  what to do.
%   Consult el_set(3), =EL_ADDFN= for details.


:- dynamic
    last_complete/2.

complete(Input, _Char, Continue) :-
    el_line(Input, line(Before, After)),
    ensure_input_completion,
    prolog:complete_input(Before, After, Delete, Completions),
    (   Completions = [One]
    ->  string_length(Delete, Len),
        el_deletestr(Input, Len),
        complete_text(One, Text),
        el_insertstr(Input, Text),
        Continue = refresh
    ;   Completions == []
    ->  Continue = refresh_beep
    ;   get_time(Now),
        retract(last_complete(TLast, Before)),
        Now - TLast < 2
    ->  nl(user_error),
        list_alternatives(Completions),
        Continue = redisplay
    ;   retractall(last_complete(_,_)),
        get_time(Now),
        asserta(last_complete(Now, Before)),
        common_competion(Completions, Extend),
        (   Delete == Extend
        ->  Continue = refresh_beep
        ;   string_length(Delete, Len),
            el_deletestr(Input, Len),
            el_insertstr(Input, Extend),
            Continue = refresh
        )
    ).

:- dynamic
    input_completion_loaded/0.

ensure_input_completion :-
    input_completion_loaded,
    !.
ensure_input_completion :-
    predicate_property(prolog:complete_input(_,_,_,_),
                       number_of_clauses(N)),
    N > 0,
    !.
ensure_input_completion :-
    exists_source(library(console_input)),
    !,
    use_module(library(console_input), []),
    asserta(input_completion_loaded).
ensure_input_completion.


%!  show_completions(+Input, +Char, -Continue) is det.
%
%   Editline command to show possible completions.

show_completions(Input, _Char, Continue) :-
    el_line(Input, line(Before, After)),
    prolog:complete_input(Before, After, _Delete, Completions),
    nl(user_error),
    list_alternatives(Completions),
    Continue = redisplay.

complete_text(Text-_Comment, Text) :- !.
complete_text(Text, Text).

%!  common_competion(+Alternatives, -Common) is det.
%
%   True when Common is the common prefix of all candidate Alternatives.

common_competion(Alternatives, Common) :-
    maplist(atomic, Alternatives),
    !,
    common_prefix(Alternatives, Common).
common_competion(Alternatives, Common) :-
    maplist(complete_text, Alternatives, AltText),
    !,
    common_prefix(AltText, Common).

%!  common_prefix(+Atoms, -Common) is det.
%
%   True when Common is the common prefix of all Atoms.

common_prefix([A1|T], Common) :-
    common_prefix_(T, A1, Common).

common_prefix_([], Common, Common).
common_prefix_([H|T], Common0, Common) :-
    common_prefix(H, Common0, Common1),
    common_prefix_(T, Common1, Common).

%!  common_prefix(+A1, +A2, -Prefix:string) is det.
%
%   True when Prefix is the common prefix of the atoms A1 and A2

common_prefix(A1, A2, Prefix) :-
    sub_atom(A1, 0, _, _, A2),
    !,
    Prefix = A2.
common_prefix(A1, A2, Prefix) :-
    sub_atom(A2, 0, _, _, A1),
    !,
    Prefix = A1.
common_prefix(A1, A2, Prefix) :-
    atom_codes(A1, C1),
    atom_codes(A2, C2),
    list_common_prefix(C1, C2, C),
    string_codes(Prefix, C).

list_common_prefix([H|T0], [H|T1], [H|T]) :-
    !,
    list_common_prefix(T0, T1, T).
list_common_prefix(_, _, []).



%!  list_alternatives(+Alternatives)
%
%   List possible completions at the current point.
%
%   @tbd currently ignores the Comment in Text-Comment alternatives.

list_alternatives(Alternatives) :-
    maplist(atomic, Alternatives),
    !,
    length(Alternatives, Count),
    maplist(atom_length, Alternatives, Lengths),
    max_list(Lengths, Max),
    tty_size(_, Cols),
    ColW is Max+2,
    Columns is max(1, Cols // ColW),
    RowCount is (Count+Columns-1)//Columns,
    length(Rows, RowCount),
    to_matrix(Alternatives, Rows, Rows),
    (   RowCount > 11
    ->  length(First, 10),
        Skipped is RowCount - 10,
        append(First, _, Rows),
        maplist(write_row(ColW), First),
        format(user_error, '... skipped ~D rows~n', [Skipped])
    ;   maplist(write_row(ColW), Rows)
    ).
list_alternatives(Alternatives) :-
    maplist(complete_text, Alternatives, AltText),
    list_alternatives(AltText).

to_matrix([], _, Rows) :-
    !,
    maplist(close_list, Rows).
to_matrix([H|T], [RH|RT], Rows) :-
    !,
    add_list(RH, H),
    to_matrix(T, RT, Rows).
to_matrix(List, [], Rows) :-
    to_matrix(List, Rows, Rows).

add_list(Var, Elem) :-
    var(Var), !,
    Var = [Elem|_].
add_list([_|T], Elem) :-
    add_list(T, Elem).

close_list(List) :-
    append(List, [], _),
    !.

write_row(ColW, Row) :-
    length(Row, Columns),
    make_format(Columns, ColW, Format),
    format(user_error, Format, Row).

make_format(N, ColW, Format) :-
    format(string(PerCol), '~~w~~t~~~d+', [ColW]),
    Front is N - 1,
    length(LF, Front),
    maplist(=(PerCol), LF),
    append(LF, ['~w~n'], Parts),
    atomics_to_string(Parts, Format).


		 /*******************************
		 *             SEARCH		*
		 *******************************/

%!  isearch_history(+Input, +Char, -Continue) is det.
%
%   Incremental search through the history.  The behavior is based
%   on GNU readline.

isearch_history(Input, _Char, Continue) :-
    el_line(Input, line(Before, After)),
    string_concat(Before, After, Current),
    string_length(Current, Len),
    search_print('', "", Current),
    search(Input, "", Current, 1, Line),
    el_deletestr(Input, Len),
    el_insertstr(Input, Line),
    Continue = redisplay.

search(Input, For, Current, Nth, Line) :-
    el_getc(Input, Next),
    Next \== -1,
    !,
    search(Next, Input, For, Current, Nth, Line).
search(_Input, _For, _Current, _Nth, "").

search(7, _Input, _, Current, _, Current) :-    % C-g: abort
    !,
    clear_line.
search(18, Input, For, Current, Nth, Line) :-   % C-r: search previous
    !,
    N2 is Nth+1,
    search_(Input, For, Current, N2, Line).
search(19, Input, For, Current, Nth, Line) :-   % C-s: search next
    !,
    N2 is max(1,Nth-1),
    search_(Input, For, Current, N2, Line).
search(127, Input, For, Current, _Nth, Line) :- % DEL/BS: shorten search
    sub_string(For, 0, _, 1, For1),
    !,
    search_(Input, For1, Current, 1, Line).
search(Char, Input, For, Current, Nth, Line) :-
    code_type(Char, cntrl),
    !,
    search_end(Input, For, Current, Nth, Line),
    el_push(Input, Char).
search(Char, Input, For, Current, _Nth, Line) :-
    format(string(For1), '~w~c', [For,Char]),
    search_(Input, For1, Current, 1, Line).

search_(Input, For1, Current, Nth, Line) :-
    (   find_in_history(Input, For1, Current, Nth, Candidate)
    ->  search_print('', For1, Candidate)
    ;   search_print('failed ', For1, Current)
    ),
    search(Input, For1, Current, Nth, Line).

search_end(Input, For, Current, Nth, Line) :-
    (   find_in_history(Input, For, Current, Nth, Line)
    ->  true
    ;   Line = Current
    ),
    clear_line.

find_in_history(_, "", Current, _, Current) :-
    !.
find_in_history(Input, For, _, Nth, Line) :-
    el_history_events(Input, History),
    call_nth(( member(_N-Line, History),
               sub_string(Line, _, _, _, For)
             ),
             Nth),
    !.

search_print(State, Search, Current) :-
    format(user_error, '\r(~wreverse-i-search)`~w\': ~w\e[0K',
           [State, Search, Current]).

clear_line :-
    format(user_error, '\r\e[0K', []).


                /*******************************
                *         PASTE QUOTED         *
                *******************************/

:- meta_predicate
    with_quote_flags(+,+,0).

add_paste_quoted(Input) :-
    current_prolog_flag(gui, true),
    !,
    el_addfn(Input, paste_quoted, 'Paste as quoted atom', paste_quoted),
    el_bind(Input, ["^Y",  paste_quoted]).
add_paste_quoted(_).

%!  paste_quoted(+Input, +Char, -Continue) is det.
%
%   Paste the selection as quoted Prolog value.   The quoting type
%   depends on the quote before the caret.  If there is no quote
%   before the caret we paste as an atom.

paste_quoted(Input, _Char, Continue) :-
    clipboard_content(String),
    quote_text(Input, String, Quoted),
    el_insertstr(Input, Quoted),
    Continue = refresh.

quote_text(Input, String, Value) :-
    el_line(Input, line(Before, _After)),
    (   sub_string(Before, _, 1, 0, Quote)
    ->  true
    ;   Quote = "'"
    ),
    quote_text(Input, Quote, String, Value).

quote_text(Input, "'", Text, Quoted) =>
    format(string(Quoted), '~q', [Text]),
    el_deletestr(Input, 1).
quote_text(Input, "\"", Text, Quoted) =>
    atom_string(Text, String),
    with_quote_flags(
        string, codes,
        format(string(Quoted), '~q', [String])),
    el_deletestr(Input, 1).
quote_text(Input, "`", Text, Quoted) =>
    atom_string(Text, String),
    with_quote_flags(
        codes, string,
        format(string(Quoted), '~q', [String])),
    el_deletestr(Input, 1).
quote_text(_, _, Text, Quoted) =>
    format(string(Quoted), '~q', [Text]).

with_quote_flags(Double, Back, Goal) :-
    current_prolog_flag(double_quotes, ODouble),
    current_prolog_flag(back_quotes, OBack),
    setup_call_cleanup(
        ( set_prolog_flag(double_quotes, Double),
          set_prolog_flag(back_quotes, Back) ),
        Goal,
        ( set_prolog_flag(double_quotes, ODouble),
          set_prolog_flag(back_quotes, OBack) )).

clipboard_content(Text) :-
    current_prolog_flag(gui, true),
    !,
    autoload_call(in_pce_thread_sync(
                      autoload_call(
                          get(@(display), paste, primary, string(Text))))).
clipboard_content("").


                /*******************************
                *           MESSAGE            *
                *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(editline(Msg)) -->
    [ 'editline: ~s'-[Msg] ].

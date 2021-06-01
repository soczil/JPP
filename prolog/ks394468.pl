% Karol Soczewica ks394468

ensure_loaded(library(lists)).

verify(N, Program) :-
    write(N),
    write('\n'),
    readFile(Program).

readFile(Filename) :- 
    set_prolog_flag(fileerrors, off),
    see(Filename),
    !,
    read(vars(Vars)),
    read(arrays(Arrays)),
    read(program(Stmts)),
    format('JOL: ~n varsy - ~p ~n arraysy - ~p ~n statementsy - ~p ~n', [Vars, Arrays, Stmts]),
    seen.
readFile(Filename, _) :-
    format('Error: plik ~p nie istnieje ~n', [Filename]).


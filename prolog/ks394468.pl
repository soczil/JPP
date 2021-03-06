% Karol Soczewica ks394468

% Stan systemu:
% Lista trzech elementów:
%   1. lista par (nazwa zmiennej, wartość);
%   2. lista par (nazwa tablicy, tablica (czyli lista));
%   3. lista obecnych pozycji każdego z procesów.

:- ensure_loaded(library(lists)).

:- op(700, xfx, <>).

% verify() == pobiera argumenty z wiersza poleceń i wywołuje na nich
% predykat verify/2
verify() :- 
    current_prolog_flag(argv, Args),
    length(Args, Len),
    (   Len < 3
    ->  format('Error: Podano za mało argumentów~n')
    ;   Args = [_, NArg, Filename | _],
        atom_number(NArg, N),
        verify(N, Filename)
    ).

% verify(+N, +Filename) == weryfikuje czy program w pliku Filename
% jest bezpieczny dla N procesów
verify(N, Filename) :-
    number(N),
    N > 0,
    !,
    readTerms(Filename, [Vars, Arrays, Stmts]),
    initState([Vars, Arrays, Stmts], N, InitialState),
    getSections(Stmts, Sections),
    (   verify(N, Stmts, Sections, [InitialState])
    ->  writeSafe()
    ;   true % informacja o błędzie została już wypisana
    ).
verify(N, _) :-
    format('Error: parametr ~p powinien byc liczba > 0~n', [N]).

% verify(+N, +Stmts, +Sections, +States) == daje w wyniku true jeśli
% program podany jako lista instrukcji Stmts jest bezpieczny dla N procesów,
% gdzie numery instrukcji sekcji krytycznych są podane w liście Sections,
% a kolejne stany do przeszukiwania DFS należą do listy States
verify(_, [], _, _). % zakładam, że pusty program jest bezpieczny
verify(N, Stmts, Sections, States) :-
    verify(N, Stmts, Sections, States, []).
verify(_, _, _, [], _) :- !. % nie ma więcej wierzchołków do odwiedzenia
verify(N, Stmts, Sections, [StateIn | T], Visited) :- % wierzchołek już odwiedzony
    member(StateIn, Visited),
    verify(N, Stmts, Sections, T, Visited),
    !.
verify(N, Stmts, Sections, [[Vars, Arrays, Positions] | T], Visited) :-
    StateIn = [Vars, Arrays, Positions],
    \+ member(StateIn, Visited),
    checkSections(Sections, Positions, InSections),
    (   safeProgram(InSections)
    ->  everyProcessStep(N, Stmts, StateIn, StatesOut),
        append(T, StatesOut, NotVisited),
        verify(N, Stmts, Sections, NotVisited, [StateIn | Visited])
    ;   getError(InSections, Error),
        writeUnsafe(Error),
        false
    ).

% getSection(+Stmts, -Sections) == Sections to lista pozycji, na których
% występuje instrukcja sekcji krytycznej.
getSections(Stmts, Sections) :- getSections(Stmts, 1, Sections).
getSections([], _, []).
getSections([Stmt | T], N, Sections) :-
    M is N + 1,
    (   Stmt = sekcja
    ->  Sections = [N | SecT],
        getSections(T, M, SecT)
    ;   getSections(T, M, Sections)
    ).

% writeSafe() == wypisuje informację o tym, że program jest bezpieczny
writeSafe() :-
    format('Program jest poprawny (bezpieczny).~n').

% writeUnsafe(+InSection) == wypisuje informację o niebezpiecznym programie
% na podstawie listy procesów będących w sekcji krytycznej w tym samym czasie
writeUnsafe([P1, P2 | _]) :-
    format('Program jest niepoprawny.~n'),
    format('Procesy w sekcji: ~p, ~p.~n', [P1, P2]).

% getError(+InSections, -Error) == Error to lista procesów,
% które w tym samym czasie są w sekcji krytycznej
getError([], []).
getError([InSection | T], Error) :-
    length(InSection, Len),
    (   Len > 1
    ->  Error = InSection
    ;   getError(T, Error)
    ).

% safeProgram(+InSections) == daje w wyniku true jeśli lista InSections
% wskazuje na to, że program jest bezpieczny i false w przeciwnym przypadku
safeProgram([]).
safeProgram([InSection | T]) :-
    length(InSection, Len),
    Len =< 1,
    safeProgram(T).

% checkSections(+Sections, +Positions, -InSections) == InSections to lista
% długości listy Sections, gdzie każdy element jest listą procesów, które
% w danym momencie znajdują się w danej sekcji krytycznej
checkSections([], _, []).
checkSections([Section | T], Positions, InSections) :-
    getProcessesInSection(Section, Positions, Processes),
    InSections = [Processes | IST],
    checkSections(T, Positions, IST).

% getProcessesInSection(+Section, +Positions, -Processes) == Processes to
% lista procesów znajdujących się w danym momencie (Positions)
% w sekcji krytycznej Section
getProcessesInSection(Section, Positions, Processes) :-
    getProcessesInSection(Section, Positions, 0, Processes).
getProcessesInSection(_, [], _, []) :- !.
getProcessesInSection(Section, [Pos | T], N, Processes) :-
    M is N + 1,
    (   Pos =:= Section
    ->  Processes = [N | ProcT],
        getProcessesInSection(Section, T, M, ProcT)
    ;   getProcessesInSection(Section, T, M, Processes)
    ).

% everyProcessStep(+N, +Stmts, +StateIn, -StatesOut) == StatesOut jest listą
% N stanów wyjściowych (po jednym dla każdego procesu), gdzie każdy taki
% stan to stan powstały poprzez wykonanie jednej, obecnej dla danego procesu
% instrukcji w stanie StateIn
everyProcessStep(0, _, _, []) :- !.
everyProcessStep(N, Stmts, StateIn, StatesOut) :-
    M is N - 1,
    step(Stmts, StateIn, M, NewState),
    StatesOut = [NewState | T],
    everyProcessStep(M, Stmts, StateIn, T).

% readTerms(+Filename, -Program) == Program jest reprezentacją programu
% wczytaną z pliku o nazwie Filename
readTerms(Filename, Program) :- 
    set_prolog_flag(fileerrors, off),
    see(Filename),
    !,
    read(variables(Vars)),
    read(arrays(Arrays)),
    read(program(Stmts)),
    Program = [Vars, Arrays, Stmts],
    seen.
readTerms(Filename, _) :-
    format('Error: plik ~p nie istnieje ~n', [Filename]).

% initState(+Terms, +N, -InitialState) == InitialState jest stanem początkowym
% programu powstałym z zainicjowania zmiennych i tablic z Terms, gdzie
% Terms to lista [Zmienne, Tablice, Instrukcje], a N to liczba procesów
initState([Vars, Arrays, _], N, InitialState) :-
    initVars(Vars, VList),
    initArrays(N, Arrays, AList),
    makeArray(N, 1, Positions),
    InitialState = [VList, AList, Positions].

% step(+Program, +StateIn, +Pid, -StateOut) == StateOut to stan powstały
% z wykonania bieżącej instrukcji z Programu (czyli listy instrukcji)
% dla procesu o identyfikatorze Pid w stanie StateIn
step(Program, [Vars, Arrays, Positions], Pid, StateOut) :-
    getArrElem(Pid, Positions, CurrentPos),
    StmtIdx is CurrentPos - 1,
    getArrElem(StmtIdx, Program, CurrentStmt),
    execStmt(CurrentStmt, [Vars, Arrays, Positions], Pid, StateOut).

% initVars(+Vars, -VList) == VList jest listą par (nazwa zmiennej, 0)
% powstałą z identyfikatorów zmiennych z listy Vars
initVars([], []).
initVars([H | T], VList) :-
    VList = [(H, 0) | VTail],
    initVars(T, VTail).

% initArrays(+N, +Arrays, -AList) == AList jest listą par
% (nazwa tablicy, lista o długości N wypełniona zerami) powstałą
% z identyfikatorów tablic z listy Arrays
initArrays(_, [], []) :- !.
initArrays(N, [H | T], AList) :-
    makeArray(N, 0, Array),
    AList = [(H, Array) | ATail],
    initArrays(N, T, ATail).

% makeArray(+N, +X, -Array) == Array to lista długości N 
% wypełniona elementami X
makeArray(0, _, []) :- !.
makeArray(N, X, Array) :-
    M is N - 1,
    Array = [X | T],
    makeArray(M, X, T).

% execStmt(+Stmt, +StateIn, +Pid, -StateOut) == StateOut jest stanem
% powstałym z wykonania instrukcji Stmt w stanie StateIn dla procesu
% o identyfikatorze Pid
execStmt(assign(Var, Expr), StateIn, Pid, StateOut) :-
    evalExpr(Expr, StateIn, Pid, Val),
    setNewValue(Var, Val, StateIn, Pid, [Vars, Arrays, Positions]),
    getArrElem(Pid, Positions, CurrentPos),
    NewPos is CurrentPos + 1,
    setArrElem(Pid, NewPos, Positions, NewPositions),
    StateOut = [Vars, Arrays, NewPositions].
execStmt(goto(N), [Vars, Arrays, Positions], Pid, StateOut) :-
    setArrElem(Pid, N, Positions, NewPositions),
    StateOut = [Vars, Arrays, NewPositions].
execStmt(condGoto(BExpr, N), [Vars, Arrays, Positions], Pid, StateOut) :-
    (   evalBoolExpr(BExpr, Pid, [Vars, Arrays, Positions])
    ->  setArrElem(Pid, N, Positions, NewPositions),
        StateOut = [Vars, Arrays, NewPositions]
    ;   getArrElem(Pid, Positions, CurrentPos),
        NewPos is CurrentPos + 1,
        setArrElem(Pid, NewPos, Positions, NewPositions),
        StateOut = [Vars, Arrays, NewPositions]
    ).
execStmt(sekcja, [Vars, Arrays, Positions], Pid, StateOut) :-
    getArrElem(Pid, Positions, CurrentPos),
    NewPos is CurrentPos + 1,
    setArrElem(Pid, NewPos, Positions, NewPositions),
    StateOut = [Vars, Arrays, NewPositions].

% setNewValue(+Var, +NewVal, +StateIn, +Pid, -StateOut) == StateOut jest
% stanem powstałym poprzez zamianę wartości dla zmiennej o identyfikatorze
% Var na wartość NewVal w stanie StateIn dla procesu o identyfikatorze Pid
setNewValue(Var, NewVal, [Vars, Arrays, Positions], _, StateOut) :-
    atom(Var),
    setVarValue(Var, NewVal, Vars, NewVars),
    StateOut = [NewVars, Arrays, Positions].
setNewValue(array(Id, E), NewVal, [Vars, Arrays, Positions], Pid, StateOut) :-
    evalExpr(E, [Vars, Arrays, Positions], Pid, Idx),
    getVarValue(Id, Arrays, Arr),
    setArrElem(Idx, NewVal, Arr, NewArr),
    setVarValue(Id, NewArr, Arrays, NewArrays),
    StateOut = [Vars, NewArrays, Positions].

% setVarValue(+Var, +NewVal, +Vars, -Res) == Res jest nową listą par
% (identyfikator, wartość) powstałą z listy Vars poprzez zamianę wartości
% elementu o identyfikatorze Var na NewVal
setVarValue(_, NewVal, [(Id, _)], [(Id, NewVal)]) :- !.
setVarValue(Var, NewVal, [(Id, Val) | T], Res) :-
    (   Id \= Var
    ->  Res = [(Id, Val) | ResT],
        setVarValue(Var, NewVal, T, ResT)
    ;   Res = [(Id, NewVal) | T]
    ).

% setArrElem(+Idx, +NewVal, +Arr, -Res) == Res jest nową listą, utworzoną
% poprzez zamianę elementu pod indeksem Idx w liście Arr na element NewVal
setArrElem(0, NewVal, [_ | T], [NewVal | T]) :- !.
setArrElem(Idx, NewVal, [H | T], Res) :-
    NewIdx is Idx - 1,
    Res = [H | ResT],
    setArrElem(NewIdx, NewVal, T, ResT).

% evalExpr(+Expr, +State, +Pid, -Res) == Res to wartość
% wyrażenia arytmetycznego Expr w stanie State dla procesu
% o identyfikatorze Pid
evalExpr(pid, _, Pid, Pid) :- !.
evalExpr(X, _, _, X) :-
    number(X),
    !.
evalExpr(Var, [Vars, _, _], _, Res) :-
    member((Var, _), Vars),
    getVarValue(Var, Vars, Res),
    !.
evalExpr(array(Id, E), [Vars, Arrays, Positions], Pid, Res) :-
    member((Id, _), Arrays),
    getVarValue(Id, Arrays, Arr),
    evalExpr(E, [Vars, Arrays, Positions], Pid, Idx),
    getArrElem(Idx, Arr, Res).
evalExpr(E1 + E2, State, Pid, Res) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res is Res1 + Res2.
evalExpr(E1 - E2, State, Pid, Res) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res is Res1 - Res2.
evalExpr(E1 * E2, State, Pid, Res) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res is Res1 * Res2.
evalExpr(E1 / E2, State, Pid, Res) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res is Res1 / Res2.

% evalBoolExpr(+BExp, +Pid, +State) == wynikiem jest true lub false
% w zależności od tego czy spełnione jest wyrażenie logiczne BExp
% w stanie State dla procesu o identyfikatorze Pid
evalBoolExpr(E1 < E2, Pid, State) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res1 < Res2.
evalBoolExpr(E1 = E2, Pid, State) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res1 =:= Res2.
evalBoolExpr(E1 <> E2, Pid, State) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res1 =\= Res2.

% getVarValue(+Var, +Vars, -Res) == Res jest wartością zmiennej Var
% w liście Vars
getVarValue(_, [(_, Val)], Val) :- !.
getVarValue(Var, [(Id, Val) | T], Res) :-
    (   Var \= Id
    ->  getVarValue(Var, T, Res)
    ;   Res = Val
    ).

% getArrElem(+Idx, +Arr, -Res) == Res jest elementem listy Arr
% pod indeksem Idx
getArrElem(0, [X | _], X) :- !.
getArrElem(Idx, [_ | T], Res) :-
    NewIdx is Idx - 1,
    getArrElem(NewIdx, T, Res).

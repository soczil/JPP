% Karol Soczewica ks394468

ensure_loaded(library(lists)).

verify(N, Filename) :-
    write(N),
    write('\n'),
    readTerms(Filename, Program),
    initState(Program, N, InitialState).

readTerms(Filename, Program) :- 
    set_prolog_flag(fileerrors, off),
    see(Filename),
    !,
    read(vars(Vars)),
    read(arrays(Arrays)),
    read(program(Stmts)),
    Program = [Vars, Arrays, Stmts],
    seen.
readTerms(Filename, _) :-
    format('Error: plik ~p nie istnieje ~n', [Filename]).

initState([Vars, Arrays, _], N, InitialState) :-
    initVars(Vars, VList),
    initArrays(N, Arrays, AList),
    makeArray(N, Positions),
    InitialState = [VList, AList, Positions].

initVars([], []).
initVars([H | T], VList) :-
    VList = [(H, 0) | VTail],
    initVars(T, VTail).

initArrays(_, [], []) :- !. % odciecie!!!!!!!!!!!!!!!!!!!!
initArrays(N, [H | T], AList) :-
    makeArray(N, Array),
    AList = [(H, Array) | ATail],
    initArrays(N, T, ATail).

makeArray(0, []) :- !. % odciecie!!!!!!!!!!!!!!!!!!!!!
makeArray(N, Array) :-
    M is N - 1,
    Array = [0 | T],
    makeArray(M, T).

% execStmt(assign(X, Expr), StateIn, Pid, StateOut) :-

evalExpr(X, _, X) :-
    number(X),
    !.
evalExpr(Var, [Vars, _, _], Res) :-
    member((Var, _), Vars),
    getVarValue(Var, Vars, Res),
    !.
evalExpr(E1 + E2, State, Res) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res is Res1 + Res2.
evalExpr(E1 - E2, State, Res) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res is Res1 - Res2.
evalExpr(E1 * E2, State, Res) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res is Res1 * Res2.
evalExpr(E1 / E2, State, Res) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res is Res1 / Res2.

evalBoolExpr(E1 < E2, State) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res1 < Res2.
evalBoolExpr(E1 = E2, State) :-
    evalExpr(E1, State, Res1),
    evalExpr(E2, State, Res2),
    Res1 =:= Res2.
% evalBoolExpr(E1 <> E2, State) :-
%     evalExpr(E1, State, Res1),
%     evalExpr(E2, State, Res2),
%     Res1 =\= Res2.

getVarValue(_, [(_, Val)], Val) :- !.
getVarValue(Var, [(Id, Val) | T], Res) :-
    (   Var \= Id
    ->  getVarValue(Var, T, Res)
    ;   Res = Val
    ).

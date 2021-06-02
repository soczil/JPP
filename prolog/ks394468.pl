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
    makeArray(N, 1, Positions),
    InitialState = [VList, AList, Positions].

step(Program, [Vars, Arrays, Positions], Pid, StateOut) :-
    getArrElem(Pid, Positions, CurrentPos),
    StmtIdx is CurrentPos - 1,
    getArrElem(StmtIdx, Program, CurrentStmt),
    execStmt(CurrentStmt, [Vars, Arrays, Positions], Pid, StateOut).

initVars([], []).
initVars([H | T], VList) :-
    VList = [(H, 0) | VTail],
    initVars(T, VTail).

initArrays(_, [], []) :- !. % odciecie!!!!!!!!!!!!!!!!!!!!
initArrays(N, [H | T], AList) :-
    makeArray(N, 0, Array),
    AList = [(H, Array) | ATail],
    initArrays(N, T, ATail).

makeArray(0, _, []) :- !. % odciecie!!!!!!!!!!!!!!!!!!!!!
makeArray(N, X, Array) :-
    M is N - 1,
    Array = [X | T],
    makeArray(M, X, T).

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
    evalBoolExpr(BExpr, [Vars, Arrays, Positions]),
    setArrElem(Pid, N, Positions, NewPositions),
    StateOut = [Vars, Arrays, NewPositions].
execStmt(sekcja, [Vars, Arrays, Positions], Pid, StateOut) :-
    getArrElem(Pid, Positions, CurrentPos),
    NewPos is CurrentPos + 1,
    setArrElem(Pid, NewPos, Positions, NewPositions),
    StateOut = [Vars, Arrays, NewPositions].

setNewValue(Var, NewVal, [Vars, Arrays, Positions], _, StateOut) :-
    setVarValue(Var, NewVal, Vars, NewVars),
    StateOut = [NewVars, Arrays, Positions].
setNewValue(array(Id, E), NewVal, [Vars, Arrays, Positions], Pid, StateOut) :-
    evalExpr(E, [Vars, Arrays, Positions], Pid, Idx),
    getVarValue(Id, Arrays, Arr),
    setArrElem(Idx, NewVal, Arr, NewArr),
    setVarValue(Id, NewArr, Arrays, NewArrays),
    StateOut = [Vars, NewArrays, Positions].

setVarValue(_, NewVal, [(Id, _)], [(Id, NewVal)]) :- !. % niby niepotrzebne!!!!!!!!
setVarValue(Var, NewVal, [(Id, Val) | T], Res) :-
    (   Id \= Var
    ->  Res = [(Id, Val) | ResT],
        setVarValue(Var, NewVal, T, ResT)
    ;   Res = [(Id, NewVal) | T]
    ).

setArrElem(0, NewVal, [_ | T], [NewVal | T]) :- !.
setArrElem(Idx, NewVal, [H | T], Res) :-
    NewIdx is Idx - 1,
    Res = [H | ResT],
    setArrElem(NewIdx, NewVal, T, ResT).

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

evalBoolExpr(E1 < E2, Pid, State) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res1 < Res2.
evalBoolExpr(E1 = E2, Pid, State) :-
    evalExpr(E1, State, Pid, Res1),
    evalExpr(E2, State, Pid, Res2),
    Res1 =:= Res2.
% evalBoolExpr(E1 <> E2, Pid, State) :-
%     evalExpr(E1, State, Pid, Res1),
%     evalExpr(E2, State, Pid, Res2),
%     Res1 =\= Res2.

getVarValue(_, [(_, Val)], Val) :- !. % niby niepotrzebne!!!!!!!!
getVarValue(Var, [(Id, Val) | T], Res) :-
    (   Var \= Id
    ->  getVarValue(Var, T, Res)
    ;   Res = Val
    ).

getArrElem(0, [X | _], X) :- !.
getArrElem(Idx, [_ | T], Res) :-
    NewIdx is Idx - 1,
    getArrElem(NewIdx, T, Res).

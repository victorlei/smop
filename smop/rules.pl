:- op(800, yfx, =>).
%:- dynamic =>/2.

N => 0 :- number(N). 
x => 1.

N*x => N.

A+B => C+D :- A => C, B => D.
A-B => C-D :- A => C, B => D.

A => A :- atomic(A),!.

[A|B] => [C|D] :-
    !,
    A => C,
    B => D.

A => G :-
    compound(A),
    !,
    compound_name_arguments(A,B,C),
    B => D,
    C => E,
    F =.. [D|E],
    F => G.
    %G is F.

% vim : syntax=prolog

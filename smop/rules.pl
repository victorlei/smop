:- op(800, yfx, =>).
%:- dynamic =>/2.

%N => 0 :- number(N). 
%X => 1.

%X*X**0 => X.
%X**1 => X. 
%X*X*.*Y => X**(Y+1).

prog([
   nBlocks = matlab_max(matlab_ravel(ai)),
   [m,n] = matlab_size(ai),
   ii = [0, 1, 0,-1],
   jj = [1, 0,-1, 0],
   a =  ai,
   mv = []]).

matlab_max(matlab_ravel(X)) =>
    fortran_max(X).

[M,N] = matlab_size(A) => 
    [M = fortran_size(A,1),
     N = fortran_size(A,2)].


len(A,1) :- atomic(A).
len(-A,K):- len(A,N), K is N+1.
len(A+B,K) :- len(A,M),len(B,N),K is M+N+1.
len(A-B,K) :- len(A,M),len(B,N),K is M+N+1.

A+0 =>  A. 
0+A =>  A.
A-0 =>  A.
0-A => -A.
%- -A => A. :- len(A,N), writeln(N).
  -0 => 0. 
 A-A => 0.
-A+A => 0.
%A+B => C+D :- A => C, B => D.
%A-B => C-D :- A => C, B => D.

% example

sin(T)**2+cos(T)**2 => 1.
A => A :- atomic(A),!.

[A|B] => [C|D] :-
    !,
    A => C,
    B => D.

A => F :-
    compound(A),
    !,
    compound_name_arguments(A,B,C),
    B => D,
    C => E, 
    %F =.. [D|E],
    compound_name_arguments(F,D,E).


% vim : syntax=prolog

prog([
   nBlocks = matlab_max(matlab_ravel(ai)),
   [m,n] = matlab_size(ai),
   ii = [0, 1, 0,-1],
   jj = [1, 0,-1, 0],
   a =  ai,
   mv = []]).


rewrite(matlab_max(matlab_ravel(X)), 
    fortran_max(X)).

rewrite([M,N] = matlab_size(A), 
    [M = fortran_size(A,1), N = fortran_size(A,2)]).

rewrite(A,A) :- atomic(A),!.

rewrite([A|B],[C|D]) :-
    !,
    rewrite(A,C),
    rewrite(B,D).

rewrite(A,F) :-
    compound(A),
    !,
    compound_name_arguments(A,B,C),
    rewrite(B,D),
    rewrite(C,E),
    compound_name_arguments(F,D,E).


% vim : syntax=prolog

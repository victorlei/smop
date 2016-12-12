:- discontiguous rank/2.
:- discontiguous shape/2.
:- discontiguous let/2.
:- dynamic let/2.
:- dynamic symtab/2.
 
:- op(800,xfy,=:).

prog([
   let(solver, matlab_function(ai,af,w)),
   let(nBlocks, matlab_max(matlab_ravel(ai))),
   let([m,n], matlab_size(ai)),
   let(ii, [0, 1, 0,-1]),
   let(jj, [1, 0,-1, 0]),
   let(a,  ai),
   let(mv, [])]).

%while(matlab_neg(matlab_isequal(af,a)),
%   []).

%let(bid, matlab_ceil(matlab_mult(matlab_rand(), nBlocks)))).
%,    %[i,j] = find(a==bid);

% resolve/2
% resolve(Expr,Result)
% Expr --> atom ; number
% let(A,B) - store B in symtab with key A
% foo([A,B...] - funcall or arrayref
% [expr1 ... exprN]
%
resolve(A) :-
    atom(A),
    assertz(is_ref(A)).

resolve(A) :-
    number(A).

resolve(let(A,B)) :-
    resolve(B),
    lhs_resolve(A).

resolve(А) :-
    compound(A),
    compound_name_arguments(A,B,C),
    resolve(B),
    resolve(C).

resolve([]).
resolve([A|B]) :-
    resolve(A),
    resolve(B).

%-----
lhs_resolve(A) :-        % A=...
    atom(A),
    assertz(is_def(A)).

lhs_resolve(A) :-
    number(A).

%lhs_resolve(let(A,B)) :- % A=B...
%    resolve(B),
%    lhs_resolve(A).

lhs_resolve(A) :-        % A(B)= ...
    compound(A),
    compound_name_arguments(A,B,C),
    lhs_resolve(B),
    resolve(C).

lhs_resolve([]).
lhs_resolve([A|B]) :-
    lhs_resolve(A),
    lhs_resolve(B).

%matlab_eval([],[]).
%matlab_eval([A|B], [C|D]) :- matlab_eval(A,C) , matlab_eval(B,D).
%
%% propagation
%const(A) :- let(A,B), const(B).
%
%shape(A,B) :- length(B,C), rank(A,C).
%shape(matlab_ceil(A),S) :- shape(A,S).
%shape(matlab_isequal(A,B),[]) :- shape(A,[]) ; shape(B,[]) ; shape(A,S), shape(B,S).
%shape(matlab_max(A),S) :- shape(A,[_|S]).
%shape(matlab_mult(A,B),S) :- shape(A,[]) ; shape(B,[]) ; shape(A,S), shape(B,S).
%shape(matlab_not(A),S) :- shape(A,S).
%shape(matlab_ravel(A,S)) :- shape(A,S).
%shape(matlab_zeros(A),A) :- is_list(A).
%
%% nested lists not implemented
%rank(A,0) :- integer(A) ; float(A) ; string(A).
%rank(A,1) :- is_list(A).
%rank(A,R) :- let(A,B), rank(B,R).
%


%   %rank(let(A,B),R) :- rank(A,R) ; rank(B,R).
%rank(matlab_size(_),1).
%rank(matlab_max(A),R-1) :- rank(A,R), R > 0.
%rank(matlab_max(A),R1) :- rank(A,R), R1 is R-1, R > 0.
%rank(A,R) :- rank(matlab_ceil(A),R).
%rank(matlab_mult(A,B),max(M,N)) :- rank(A,M), rank(B,N).
%rank(matlab_mult(A,B),R) :- rank(A,M), rank(B,N), R is max(M,N).
%rank(matlab_mult(A,B),R) :- rank(A,M), rank(B,N), R=M*N.
%rank(matlab_rand,0).
% 1. resolve (def/use, funcall/arrayref)
% 2. rank/shape/const
% 3. optimize/resolve
% 4. code gen

% vim : syntax=prolog

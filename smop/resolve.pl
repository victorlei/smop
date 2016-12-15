%:- discontiguous is_def/1.
%:- discontiguous is_def/1.
%:- discontiguous shape/2.
%:- discontiguous let/2.
:- dynamic is_def/1.
:- dynamic is_ref/1.
:- dynamic resolve/1.
:- dynamic do_resolve/1.
:- dynamic lhs_resolve/1.
 
prog([
   let(solver, matlab_function(ai,af,w)),
   let(nBlocks, matlab_max(matlab_ravel(ai))),
   let([m,n], matlab_size(ai)),
   let(ii, [0, 1, 0,-1]),
   let(jj, [1, 0,-1, 0]),
   let(a,  ai),
   let(mv, [])]).

% b-code down       stack grows right
% +             +           f      |       |
% 2             + 2         f      | x     |
% 3             + 2 3       f      | x     | y
% []            + [2,3]     f      | [x,y] |
% ()            5           f(x,y) |       |

% ?- do_resolve(a=b).
% =
% a
% b
% []
% ()

% () apply/2
% [] list/_
% =/2
% TODO
% 0. Copy state of is_def/is_ref  --> resolve statements
% 4, const rank shape
% 6. SSA
% 8. Macroexpand
% 10. parser
% 12. backend
do_resolve(A) :-
    %retractall(is_def(_)),
    %retractall(is_ref(_)),
    resolve(A).
    %listing(is_def/1),
    %listing(is_ref/1).

is_unused(A) :-
    is_def(A),
    \+ is_ref(A).

is_arrayref(A) :-
    is_def(A).

is_funcall(A) :- 
    is_ref(A), 
    \+ is_def.
    
resolve(A) :-
    atom(A),
    !,
    writeln(A),
    assertz(is_ref(A)).

resolve(A) :-
    number(A),
    !,
    writeln(A).

resolve(let(A,B)) :-
    !,
    resolve(B),
    lhs_resolve(A),
    writeln(let).

resolve([]) :-
    !,
    writeln("[]").

resolve([A|B]) :-
    !,
    resolve(A),
    resolve(B).

resolve(A) :-
    compound(A),
    !,
    compound_name_arguments(A,B,C),
    resolve(B),
    resolve(C),
    writeln("()").
%--------------------------------
lhs_resolve(A) :-        % A=...
    atom(A),
    !,
    writeln(A),
    assertz(is_def(A)).

%lhs_resolve(A) :-
%    number(A).

lhs_resolve(let(A,B)) :- % A=B...
    !,
    resolve(B),
    lhs_resolve(A),
    writeln(let).

lhs_resolve([]) :-
    !,
    writeln("[]").

lhs_resolve([A|B]) :-
    !,
    lhs_resolve(A),
    lhs_resolve(B).

lhs_resolve(A) :-        % A(B)= ...
    compound(A),
        !,
    compound_name_arguments(A,B,C),
    lhs_resolve(B),
    resolve(C),
    writeln("()").

% vim : syntax=prolog

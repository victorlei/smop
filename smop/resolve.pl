:- dynamic is_definition/1.
:- dynamic is_reference/1.
:- dynamic resolve/1.
:- dynamic do_resolve/1.
:- dynamic lhs_resolve/1.

:- op(800,xfy, (=.)).

prog([
   solver =. matlab_function(ai,af,w),
   nBlocks =. matlab_max(matlab_ravel(ai)),
   [m,n] =. matlab_size(ai),
   ii =. [0, 1, 0,-1],
   jj =. [1, 0,-1, 0],
   a =.  ai,
   mv =. []]).

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

name_ident(Name,Ident) :-
    % Given a Name, create ident(Name,Addr)
    % having brand new Addr.
    gensym('',Atom),
    atom_number(Atom,Addr),
    Ident=ident(Name,Addr),
    writeln(Ident).

cleanupall :-
    retractall(is_reference(Ident)),
    retractall(is_definition(Ident)).

do_resolve(A) :-
    resolve(A).

resolve(Name) :-
    atom(Name),
    !,
    name_ident(Name,Ident),
    assertz(is_reference(Ident)).

resolve(A) :-
    number(A),
    !,
    writeln(A).

resolve(A =. B) :-
    !,
    resolve(B),
    lhs_resolve(A),
    writeln(=.).

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

lhs_resolve(Name) :-        % A=...
    atom(Name),
    !,
    name_ident(Name,Ident),
    assertz(is_definition(Ident)).

%lhs_resolve(A) :-
%    number(A).

lhs_resolve(A =. B) :- % A=B...
    !,
    resolve(B),
    lhs_resolve(A),
    writeln(=.).

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

has_definitions(Name,AddrList) :-
    findall(Addr, is_definition(ident(Name,Addr)), AddrList).

% vim : syntax=prolog

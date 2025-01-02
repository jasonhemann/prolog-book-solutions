
%% Dynamic flags so that I can use predicate_property/2 on the input
:- dynamic(a/1).
:- dynamic(b/1).
:- dynamic(c/1).
:- dynamic(d/1).
:- dynamic(e/1).
:- dynamic(f/1).
:- dynamic(g/1).
:- dynamic(h/1).

trace(true) :-
    !, write('True'), nl.
trace((A, B)) :-
    !, trace(A), trace(B).
trace((A ; B)) :-
    !, (trace(A) ; trace(B)).
trace(!) :-
    !, write('Cut'), nl.
trace(G) :-
    \+ predicate_property(G, built_in),
    write('Entering: '), writeq(G), nl,
    clause(G, Body), % Find the clause for G
    trace(Body),
    write('Exiting: '), writeq(G), nl.
trace(G) :-
    predicate_property(G, built_in),
    write('Calling built-in: '), writeq(G), nl,
    call(G),
    write('Succeeded built-in: '), writeq(G), nl.
trace(G) :-
    write('Failed: '), writeq(G), nl, fail.


% Unlike Lucas's Prolog system, scryer does not allow atoms as heads
% of rules. So I had to modify the program.
%
% This also corrects a bug in the original program

a(a) :- b(b), c(c), d(d).

b(b) :- e(e), !, f(f).
b(b) :- g(g), h(h).

f(f) :- i.
f(f) :- j.

c(c) :- fail.
e(e) :- fail.
g(g) :- fail.
h(h) :- fail.

i.
j.

%% ?- trace(a(a)).

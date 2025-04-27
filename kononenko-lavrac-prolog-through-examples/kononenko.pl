
:- use_module(library(debug)).
:- use_module(library(clpz)).
:- use_module(library(debug)).
:- use_module(library(reif)).
:- use_module(library(clpb)).
:- use_module(library(dif)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(si)).
:- use_module('../../constrained.pl/constrained').
:- use_module(library(debug)).

%% Prolog Through Examples
%% Kononenko and Lavrac

%% Chapter 1

%% §1.2

child(ann,mary).
child(peter,ann).
child(jane,ann).

parent(X,Y) :- child(Y,X).

divisor(X, X, X) :- #X #>= 1.
divisor(X, Y, Z) :-
	#X #>= 1, #Y #>= 1, #Z #>= 1, X #> Y, Y #>= Z, #W #= X - Y,
	divisor0(Y,Z,W).

divisor0(Y,Z,W) :- W #>= Y, divisor(W, Y, Z).
divisor0(Y,Z,W) :- W #<Y, divisor(Y, W, Z).

%% ?- length(Ls,L), #L #= #X + #Y, divisor(X,Y,Z), Z = 3.
%%    Ls = [_A,_B,_C,_D,_E,_F], L = 6, X = 3, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I], L = 9, X = 6, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L], L = 12, X = 9, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O], L = 15, X = 12, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O], L = 15, X = 9, Y = 6, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R], L = 18, X = 15, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T|...], L = 21, X = 18, Y = 3, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T|...], L = 21, X = 15, Y = 6, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T|...], L = 21, X = 12, Y = 9, Z = 3
%% ;  Ls = [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T|...], L = 24, X = 21, Y = 3, Z = 3
%% ; ... .
%% ?- divisor(0,0,X).
%%    X = 0
%% ;  false.
%% ?- divisor(24,0,X).
%%    X = 24
%% ;  false.
%% ?- divisor(0,24,X).
%%    X = 24
%% ;  false.
%% ?-

%% Chapter 2

%% Exercise 2.1

:- discontiguous(play/2).
:- discontiguous(man/1).
:- discontiguous(mortal/1).

play(judith,piano).

man(john).

mortal(john).
mortal(_X).

sits_beside([peter,ann],judith).

%% Exercise 2.2

:- discontiguous(head/2).

head(mr_brown,mr_richards).
teacher(mr_richards,tom).
teacher(mr_richards,ann).

%% §2.2

%% Exercise 2.4

play(judith,X) :- has_keyboard(X).
play(judith,X) :-
	play(peter,X),
	play(tom,X).
play(judith,X) :-
	play(peter,X)
	;
	play(tom,X).

:- discontiguous(likes/2).

likes(john,X) :- likes(X,wine).

%% Exercise 2.5

%% All men are mortal
mortal(X) :- man(X).

%% All men like all women.
likes(X,Y) :- man(X), woman(Y).

%% All men like one woman
%%
%% Skolemized the variable, represented as a special constant 'w'.
likes(X,w) :- man(X).

%% Each man likes a woman.
%%
%% Requires an actual Skolem function to describe the
%% woman.
likes(X,woman_of(X)) :- man(X).

%% Some people are smart.
%%
%% Skolem constant einstein.
person(einstein).
smart(einstein).

%% Nothing is good and bad at the same time.
%%

%% ASP Solution
%%% Exactly one valence for each object X:
%% { good(X) ; bad(X) } = 1 :- object(X).

%% Exercise 2.10

%% ?- play(X,_).
plays_piano_accordion:- play(X,piano), play(X,accordion).
%% ?- play(_,_).

%% §2.5

mother(Mom,C) :- sex(Mom,female), parent(Mom,C).

son(Son,Parent) :- sex(Son,male), parent(Parent,Son).


%% The following will terminate because there are no recursive rules,
%% just facts.

grandfather(Gfather,Gchild) :-
	sex(Gfather,male),
	parent(Gfather,Child),
	parent(Child,Gchild).


uncle(Uncle,Nibling) :-
	sex(Uncle,male),
	parent(GP,Uncle),
	parent(GP, X),
	dif(Uncle,X),
	parent(X,Nibling).

cousin(C1,C2) :-
	dif(C1,C2), %% Important for some families.
	parent(P1,C1),
	dif(P1,P2),
	parent(P2,C2),
	parent(GP,P1),
	parent(GP,P2).

%% Exercise 2.12

pupil(P,T) :- teacher(T,P).

head(Hmaster,P) :- head(Hmaster,T), teacher(T,P).

superior(X,Y) :- head(X,Y).
superior(X,Z) :- head(X,Y), superior(Y,Z).

%% Chapter 3

%% Exercise 3.1

%% ?- person(tom,Age,Address).
%% ?- person(ann,Age,_).
%% ?- findall(Name,person(Name,10,_),Solutions).
%% ?- findall(Name,person(Name,10,'Jamova 39'),Solutions).
%% ?- findall(Name-Address,person(Name,_,Address),Solutions).
%% ?- person(_,_,_).

%% Exercise 3.3

flight(_Flight_num,relation(_From,_To),departure(_Day,time(_Hour,_Minute))).

%% Exercise 3.4

wrote(tom,sentence(pronoun(i),verb(love),pronoun(you))).

%% ?- wrote(tom,X).
%%    X = sentence(pronoun(i),verb(love),pronoun(you)).
%% ?- wrote(tom,sentence(I,Love,You)).
%%    I = pronoun(i), Love = verb(love), You = pronoun(you).
%% ?- wrote(Who,sentence(_,verb(Does),pronoun(Whom))).
%%    Who = tom, Does = love, Whom = you.
%% ?-

%% §3.3

%% Instead of name/2

%% ?- atom_codes(X,[80, 82, 79, 76, 79, 71]).
%%    X = 'PROLOG'.

%% Exercise 3.8

predicate(foo,3).

%% I want to use the constrained.pl constraints
%%
%% But I believe in order to make 'variable' a distinguished term and
%% produce different answers for either, I'd have to modify the
%% implementation to carry that distinction through it.

term(variable).
term(F) :- functor_c(F,Tag,_Arity,Args), dif(Tag,variable), *(maplist(term,Args)). %%

atomic_formula(P) :- functor_c(P,Tag,Arity,Args), predicate(Tag,Arity), *(maplist(term,Args)). %%

literal(L) :- atomic_formula(X), (L = X ; L = not(X)).

%% §3.4

%% Exercise 3.9

%% country(Name,Population,SquareMiles).
country(usa,300_000_000,3_000_000).

density(Name,Density) :- #Density * #Area #= #Population, country(Name,Population,Area).

%% Exercise 3.10

p(X,Y) :- #Y #= #_D * X.

%% ?- p(7,21).
%%    true.
%% ?- p(3,10).
%%    false.
%% ?- p(6,30).
%%    true.
%% ?- p(6,35).
%%    false.
%% ?- p(X,21).
%%    clpz:(X in-21.. -1\/1..21), clpz:(_A*X#=21), clpz:(_A in-21.. -1\/1..21).

%% Chapter 4

%% https://asp-chef.alviano.net/s/sunny-day#jasonhemann/jasonhemann-asp-chef-short-links

%% Exercise 4.3

%% The program child/2 through parent/2, and parent/2 indirectly through child/2 --- its cyclic.



%% Exercise 4.7

%% ?- use_module(library(pairs)).
%% true.
%% ?- [user].
%% concat(X-Y,Y-Z,X-Z).

%% ?- concat([a,b,c|X]-X,[d,e|Y]-Y,Z).
%%    X = [d,e|Y], Z = [a,b,c,d,e|Y]-Y.
%% ?- concat([a|X]-X,Y,[a,b,c,d,e|Z]-Z).
%%    X = [b,c,d,e|Z], Y = [b,c,d,e|Z]-Z.
%% ?- concat([1,2|X]-X, [3,4|Y]-Y,Z), concat(Z, [5|W]-W,R).
%%    X = [3,4,5|W], Y = [5|W], Z = [1,2,3,4,5|W]-[5|W], R = [1,2,3,4,5|W]-W.

%% Exercise 4.8

%% Perhaps error on non-list types, just for an alternative.
empty([]).


%% Exercise 4.9

%% I believe what the author wants here is the missing disequality constraint.
%% Added male predicate to make problem go. 

male(X) :- man(X).

brother(X,Y) :-
	dif(X,Y),
	parent(Z, X),
	parent(Z, Y),
	male(X).

%% Exercise 4.13

d([], 0).
d([_|L],s(D)) :- d(L,D).


%% (b)
%% ?- d(L,s(s(s(0)))).
%%    L = [_A,_B,_C].

%% JBH: Neat that it underscores the i-dont-care variables!

%% Exercise 4.14

inv(X,X).
inv(X + Y, Y1 + X1) :-
	inv(X, X1), inv(Y, Y1).

%% ?- inv(1 + 2 + 3, Z).
%%    Z = 1+2+3
%% ;  Z = 3+(1+2)
%% ;  Z = 3+(2+1)
%% ;  false.
%% ?- inv(1 + (2 + 3), Z).
%%    Z = 1+(2+3)
%% ;  Z = 2+3+1
%% ;  Z = 3+2+1
%% ;  false.
%% ?-

%% Exercise 4.15

% (c) Member1 is better, but both suck.

%% Better
member3(X,[X|_]).
member3(X,[Y|Res]) :- dif(X,Y), member3(X,Res).

%% Or, instead.
member4(X,[Y|Res]) :- if_(X = Y, true, member4(X,Res)).

%% Exercise 4.16

%% Exchanging the clauses in the given program would prevent fresh milk from ever being issued.
%% That's as much a flaw in the original program as an issue of the clause ordering

issue(milk,Litres) :-
	stock(milk,fresh,L),
	if_(#Litres #< #L,
		(#L0 #= #L - #Litres,
		 retract(stock(milk,fresh,L)),
		 asserta(stock(milk,fresh,L0)),
		 assertz(issued(milk,fresh,Litres))),
		assertz(issued(milk,powdered,Litres))).


%% ?- abolish(issued/3).
%%    true.
%% ?- abolish(stock/3).
%%    true.
%% ?- [kononenko].
%%    true.
%% ?- asserta(stock(milk,fresh,1000)).
%%    true.
%% ?- issue(milk,10).
%%    true.
%% ?- stock(milk,fresh,X).
%%    X = 990.
%% ?- issue(milk,100).
%%    true.
%% ?- issue(milk,1010).
%%    true.
%% ?- issued(milk,X,Y).
%%    X = fresh, Y = 10
%% ;  X = fresh, Y = 100
%% ;  X = powdered, Y = 1010.
%% ?-

%% Exercise 4.17

% The correct program uses the clpz `max` arithmetic operator.

%% Exercise 4.22

%% See our solution to 4.16

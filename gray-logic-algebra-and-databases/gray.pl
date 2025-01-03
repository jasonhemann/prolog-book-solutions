:- set_prolog_flag(occurs_check,error).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- initialization(asserta(clpz:monotonic)).

:- discontiguous(price/2).

%% Logic, Algebra, and Databases
%% Peter Gray

%% Ch. 4

%% §4.2

between_gray(X,L,U) :- between(L,U,X).

%% §4.3

price(X,30) :- #X #>= 6.
price(X,20) :- #X #>= 4, #X #< 6.
price(X,10) :- #X #< 4.

%% §4.4

supplies(wilco,widgets).
supplies(bronco,screws).
supplies(elco,toolkits).
supplies(elco,washers).
supplies(X,screws) :- supplies(X,toolkits).

price(90,widgets).
price(25,toolkits).
price(10,screws).
price(15,washers).

%% ?- setof([X,Y,P],(supplies(X,Y), price(P,Y), #P #< 20),Solutions).
%%    Solutions = [[bronco,screws,10],[elco,screws,10],[elco,washers,15]].

%% §4.5

app([],L,L).
app([X|L1],L2,[X|R]) :- app(L1,L2,R).

%% §4.7.3

employs(cIBM,person(fred,25,"m")).
forename(person(X,_Y,_Z),X).
age(person(_X,Y,_Z),Y).
sex(person(_X,_Y,Z),Z).

%% §4.7.4

%% ?- forename(P,fred),sex(P,"m"),age(P,25),employs(C,P).
%%    P = person(fred,25,"m"), C = cIBM.

%% §4.10

lmax(L,R) :- list_max(L,R).

greater(X,Y,R) :- #R #= max(#X,#Y).

%% §4.11

mem(X,[X|_L]).
mem(X,[H|L]) :- dif(X,H), mem(X,L).


listp([],L) :- numlist(6,L0), reverse(L0,L).

lset_difference([],L,L).
lset_difference([X|Rest],Input,Output) :-
	lset_difference(Rest,Input,Res),
	tfilter(dif(X),Res,Output).

lset_intersect(L1,L2,L) :-
	lset_difference(L1,L2,L3),
	lset_difference(L2,L3,L).

%% lset_union(L1,L2,L) :-
%% 	lset_difference(L1,L2,L3),
%% 	append(L3,L2,L).

%% Fuses the append and set_diff
%%
lset_union([],L,L).
lset_union([X|L1],L0,[X|L]) :-
	lset_union(L1,L0,L2),
	tfilter(dif(X),L2,L).


%% §4.12


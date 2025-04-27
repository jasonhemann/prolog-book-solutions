:- set_prolog_flag(occurs_check,error).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(iso_ext)). % copy_term, e.g.
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- initialization(asserta(clpz:monotonic)).

%% Chapter 1

:- discontiguous(car/7).

% car(Make,Model,Trim,Country,Cc,BodyStyle,Cost).
car(ford,fiesta,popular,uk,950,hatchback,5300).
car(ford,orion,gl,uk,1300,saloon,7800).
car(ford,orion,gl,uk,1600,saloon,8600).
car(ford,orion,ghia,uk,1600,saloon,9500).
car(fiat,uno,55,italy,950,hatchback,5200).
car(fiat,uno,70,italy,1050,hatchback,6500).
car(rover,metro,city,uk,1000,hatchback,4900).
car(rover,metro,mg,uk,1300,hatchback,5700).

%% ?- car(Man,_,_,_,1300,_,_).
%%    Man = ford
%% ;  Man = rover.
%% Only works b/c no repetitions

%% ?- findall(X-Y,car(X,Y,_,_,_,_,_),Solutions).
%%    Solutions = [ford-fiesta,ford-orion,ford-orion,ford-orion,fiat-uno,fiat-uno,rover-metro,rover-metro].

%% X and W are keys b/c they are in the pattern but not in the template.
%% U would be a key also, except that it's listed prefixed (U^) in front of of the goal.
%%
%% The third argument is the filter. Only those values that unify with
%% [[metro|Other]] are printed out, b/c again unification, and only
%% the Other variable, representing the cdr of the list, is printed.

%% ?- setof([Y,Z,V],U^car(X,Y,Z,W,V,U,_),[[metro|Other]]).
%%    X = rover, W = uk, Other = [city,1000]
%% ;  X = rover, W = uk, Other = [mg,1300].

%% So the solution I wanted to come up with was:
abc(X,Y,_) :- car(X,Y,_,_,_,_,_).
%% ?- setof(X-Y, abc(X,Y,Z), Solution).
%% Seem to have a problem w/lambdas
%% setof(X-Y, \X^Y^Z^car(X,Y,_,_,_,_,_), Solution).

supplier(ford,uk,'21 Tingate, Dagenham','0181 233 4821').
supplier(rover,uk,'18 Beadle Road, Cowley','01325 24112').
supplier(fiat,italy,'333 Via Alphonse, Turin','0101 888 376 3983').


%% Chapter 2

risk_for_capacity(Capacity,1) :- #Capacity #< 1000.
risk_for_capacity(Capacity,2) :- #Capacity #< 1300, #Capacity #>= 1000.
risk_for_capacity(Capacity,3) :- #Capacity #< 1500, #Capacity #>= 1300.
risk_for_capacity(Capacity,4) :- #Capacity #< 2000, #Capacity #>= 1500.
risk_for_capacity(Capacity,5) :- #Capacity #< 3500, #Capacity #>= 2000.
risk_for_capacity(Capacity,6) :- #Capacity #>= 3500.


%% ?- car(Man,Mod,Trim,_,Cc,_,_), risk_for_capacity(Cc,Points).
%%    Man = ford, Mod = fiesta, Trim = popular, Cc = 950, Points = 1
%% ;  Man = ford, Mod = orion, Trim = gl, Cc = 1300, Points = 3
%% ;  Man = ford, Mod = orion, Trim = gl, Cc = 1600, Points = 4
%% ;  Man = ford, Mod = orion, Trim = ghia, Cc = 1600, Points = 4
%% ;  Man = fiat, Mod = uno, Trim = 55, Cc = 950, Points = 1
%% ;  Man = fiat, Mod = uno, Trim = 70, Cc = 1050, Points = 2
%% ;  Man = rover, Mod = metro, Trim = city, Cc = 1000, Points = 2
%% ;  Man = rover, Mod = metro, Trim = mg, Cc = 1300, Points = 3
%% ;  false.

price_and_address(Man,Mod,Trim,Cap,Price,Address) :-
	car(Man,Mod,Trim,Origin,Cap,_,Price),
	suppliers(Man,Origin,Address,_).

%% Assume we demand full integer prices.
discount_for_cash(Fullprice,Cutprice) :- #Fullprice * 9 #= #Cutprice * 10.


% Exercises 2

% 2.4

event(battle_of_hastings,1066).
event(plague_of_london,1665).
event(fire_of_london,1666).
event(man_on_the_moon,1969).

happened_before(E1,E2) :-
	event(E1,Y1),
	#Y1 #< #Y2,
	event(E2,Y2).

% 2.5

year(pete,1).
year(nigel,1).
year(sam,3).
year(jane,3).
year(tom,N) :- N in 1..4.
year(dave,N) :- N in 1..4.

hard_worker(pete).
hard_worker(nigel).
hard_worker(jane).

likes(sam,tom).
likes(pete,tom).
likes(jane,dave).
likes(dave,X) :- hard_worker(X), year(X,1).

dislikes(sam,dave).
dislikes(pete,dave).
dislikes(tom,X) :- year(X,3), likes(X,dave).

% Note this does not capture the mutual exclusivity of likes and dislikes.

% Cf the ASP solution
%
%https://asp-chef.alviano.net/s/pete-likes#jasonhemann/jasonhemann-asp-chef-short-links

% 2.6

car(ford,capri,injection,uk,2800,sports,11200).
car(alfa_romeo,sprint,veloce,italy,2000,coupe,12500).
car(volvo,928,gls,sweden,1400,hatchback,6290).
car(mitsubishi,colt,glx,japan,1800,estate,7420).
car(mercedes,roadster,280,germany,2800,convertible,18450).

risk_for_bodystyle(estate,1).
risk_for_bodystyle(saloon,2).
risk_for_bodystyle(hatchback,1).
risk_for_bodystyle(sports,3).
risk_for_bodystyle(coupe,5).
risk_for_bodystyle(convertible,7).

% 2.7

total_risk(Cc,BodyStyle,N) :- risk_for_capacity(Cc,N1), risk_for_bodystyle(BodyStyle,N2), #N #= #N1 + #N2.

%% In order to get the correct answers w/o repeats, we'd have to setof.
%% Collect together the pieces we don't care about into KV, and select the ones we do.

%% ?- setof(Trim-Cost,(car(Make,Model,Trim,Country,Cc,BodyStyle,Cost),total_risk(Cc,BodyStyle,Risk)),TrimCost).
%%    Make = alfa_romeo, Model = sprint, Country = italy, Cc = 2000, BodyStyle = coupe, Risk = 10, TrimCost = [veloce-12500]
%% ;  Make = fiat, Model = uno, Country = italy, Cc = 950, BodyStyle = hatchback, Risk = 2, TrimCost = [55-5200]
%% ;  Make = fiat, Model = uno, Country = italy, Cc = 1050, BodyStyle = hatchback, Risk = 3, TrimCost = [70-6500]
%% ;  Make = ford, Model = capri, Country = uk, Cc = 2800, BodyStyle = sports, Risk = 8, TrimCost = [injection-11200]
%% ;  Make = ford, Model = fiesta, Country = uk, Cc = 950, BodyStyle = hatchback, Risk = 2, TrimCost = [popular-5300]
%% ;  Make = ford, Model = orion, Country = uk, Cc = 1300, BodyStyle = saloon, Risk = 5, TrimCost = [gl-7800]
%% ;  Make = ford, Model = orion, Country = uk, Cc = 1600, BodyStyle = saloon, Risk = 6, TrimCost = [ghia-9500,gl-8600]
%% ;  ... .

%% Chapter 3.

fact(0,1).
fact(N,N_fact) :-
	#N #> 0,
	#M #= #N - 1,
	#N_fact #= #N * #M_fact,
	fact(M, M_fact).

:- discontiguous(child_father/2).
:- discontiguous(child_mother/2).

% Exercises 3

% 3.2

% Corrects a (probable) mistake in the data.
%
% Renames predicates along the lines suggested by mtriska in
% https://www.youtube.com/watch?v=Uska9DgJEoo

% First generation
child_father(charles, philip).
child_mother(charles, elizabeth).

% Second generation
child_father(henry, charles).
child_mother(henry, diana).

child_father(william, charles).
child_mother(william, diana).

% Third generation (children of William)
child_father(george, william).
child_mother(george, catherine).  % Catherine (Kate Middleton)
child_father(charlotte, william).
child_mother(charlotte, catherine).
child_father(louis, william).
child_mother(louis, catherine).

% Third generation (children of Harry)
child_father(archie, henry).
child_mother(archie, meghan).  % Meghan Markle
child_father(lilibet, henry).
child_mother(lilibet, meghan).

ancestor(C,F) :- child_father(C,F).
ancestor(C,M) :- child_mother(C,M).
ancestor(C,A) :- child_father(C,F), ancestor(F,A).
ancestor(C,A) :- child_mother(C,M), ancestor(M,A).

% 3.3

mod5(X,Y) :- #Y #= #X mod 5.

% 3.4

power(P,X,N) :- #P #= #X ^ #N.

% 3.5

mins_to_hours_and_mins(Total,Hours,Mins) :- #Hours #= #Total // 60, #Mins #= #Total rem 60.

%% ?- mins_to_hours_and_mins(-100,X,Y).
%%    X = -1, Y = -40.
%% ?- mins_to_hours_and_mins(30,X,Y).
%%    X = 0, Y = 30.
%% ?- mins_to_hours_and_mins(305,X,Y).
%%    X = 5, Y = 5.


%% Chapter 4

%% I change his book code here to using asserta rather than assertz
%% This better maintains stack discipline.
incr_count(G) :- call(G), retract(count(N)), #M #= 1 + #N, asserta(count(M)), fail.
incr_count(_).

count(G,N) :-
  asserta(count(0)),
  incr_count(G),
  retract(count(N)).


%% §4.7

%% NB. yfy is not an operator_specifier in scryer
%%
%% ?- current_op(X,yfy,Z).
%%    error(domain_error(operator_specifier,yfy),op/3).

%% Exercises 4

% 4.1

%% Why do it this way?
%%
%% We have a literal backtracking stack-oriented discipline here.

data(item1).
data(item2).
data(item3).

%% ?- data(X).

% 4.2

% Oh, I guess I see what he wants here.

%% If you wanted them as data, use a list? Why not represent your
%% stack with lists, and get the elements out as you need them with
%% pattern matching?

stack(E,Stk,[E|Stk]).

%% ?- stack(item1,[],Stk0),
%%    stack(item2,Stk0,Stk1),
%%    stack(item3,Stk1,Stk2),
%%    length(Stk2,N),
%%    length(Stk1,N2).

% 4.3

%% No. I simulate imperative behaviors functionally, so there's no
%% need.

% 4.4

%% No need. Declarative programming with CLP(Z) makes this
%% unnecessary.

%% §5.1

%% Reformulated problem to Tax_percent.
%%
%% WLOG, assume government leaves you the remainder.

get_tax(salary(Gross,Tax_percent), Tax_amount) :-
	(#Gross * #Tax_percent) div 100 #= #Tax_amount.

%% ?- get_tax(salary(Gross,10),8500).
%%    clpz:(Gross in 85000..85019), clpz:(#Gross*10#= #_A), clpz:(_A in 850000..850190), clpz:(#_B+ #_C#= #_A), clpz:(#_A mod 100#= #_C), clpz:(_B in 850000..850099), clpz:(_C in 0..99).

%% Lucas's arg/3 is our nth0/3.
%% assumes library(lists).

%% ?- electrics(battery,starter,lights) =.. [F|_A], nth0(2,_A,E).
%%    F = electrics, _A = [battery,starter,lights], E = lights.

%% §5.4

%% Lucas suggests a management tree example example. He has us write
%% out a binary management tree as
%% management_tree(adams,manages(brown,collins)).
%%
%% I don't understand the point of tagging the subordinates and using
%% indirection.
%%
%% Is the idea that I don't want to have to write my data as one fact?
%% Why would that be a problem?

binary_tree(adams,
			binary_tree(brown,
						binary_tree(dorking,null,null),
						binary_tree(evans,null,null)),
			binary_tree(collins,
						binary_tree(fortnum,null,null),
						binary_tree(gault,null,null))).

%% Even doing so, I'm not sure I see the benefit to the extra tagging.

management_tree(adams,manages(brown,collins)).
management_tree(brown,manages(dorking,evans)).
management_tree(collins,manages(fortnum,gault)).
management_tree(dorking,manages(null,null)).
management_tree(evans,manages(null,null)).
management_tree(fortnum,manages(null,null)).
management_tree(gault,manages(null,null)).

%% Implementing a variant without the side effects of the original
%% program.
%%
%% The original would also break on Mr. Null.

%% This assumes every 'real data' leaf is actually has some
%% management_tree(Name,null,null). Makes it easier to connect onto.

l_r_preorder(Name) -->
    [Name],
    { management_tree(Name,manages(L,R)) },
    l_r_preorder_include_subtree(L),
    l_r_preorder_include_subtree(R).

l_r_preorder_include_subtree(Child) -->
    { dif(Child, null) },
    l_r_preorder(Child).
l_r_preorder_include_subtree(null) --> [].

% ?- phrase(l_r_preorder(Name), List).

%% Exercises 5b

%% 5b.2

%% computer(cpu(Make,Model),Memory,disk(Type,Capacity),monitor(Resolution,Color)).
% computer(cpu(intel,6502),Memory,disk(Type,Capacity),monitor(Resolution,Color)).


%% 5b.3

l_r_postorder(Name) -->
	{ management_tree(Name,manages(L,R)) },
	l_r_postorder_include_subtree(L),
	l_r_postorder_include_subtree(R),
	[Name].

l_r_postorder_include_subtree(Child) -->
    { dif(Child, null) },
    l_r_postorder(Child).
l_r_postorder_include_subtree(null) --> [].

% ?- phrase(l_r_postorder(Name), List).

%% 5b.4

% Rather than printing to screen, we produce the tree structure as a value.

org_chart(Name,manages(Name,null,null)) :- management_tree(Name,manages(null,null)).
org_chart(Name,manages(Name,manages(Name0,Subtree0),manages(Name1,Subtree1))) :-
	dif(Name0,null),
    dif(Name1,null),
	management_tree(Name,manages(Name0,Name1)),
	org_chart(Name0,Subtree0),
	org_chart(Name1,Subtree1).

% ?- org_chart(adams,X), write_canonical(X).

%% §6.3

%% mem(?Left, ?Right, ?Combination)
%% This member uses `if_` instead of explicit `dif` instead of cut.
mem(X,[Y|R],Z) :- if_(X = Y, Z = [X|R], mem(X,R,Z)).

%% §6.6

% Implemented mergesort instead of bubblesort, sorting works with
% either list input.

% mergesort(+Goal, ?List, ?Sorted)
% Succeeds if Sorted is the sorted version of List.
% Assumes that List is a list of integers.
mergesort(_M, [], []).
mergesort(_M, [X], [X]).
mergesort(M, [X,Y|Rest], [A,B|Rest2]) :-
    split([X,Y|Rest], [X|LeftHalf], [Y|RightHalf]),
    mergesort(M, [X|LeftHalf], SortedLeft),
    mergesort(M, [Y|RightHalf], SortedRight),
    call(M, SortedLeft, SortedRight, [A,B|Rest2]).

% split(?List, ?Left, ?Right)
% Splits the given list into two halves, Left and Right.
split([], [], []).
split([X], [X], []).
split([X,Y|Rest], [X|A], [Y|B]) :- split(Rest, A, B).

% merge(+Goal, +Goal, ?SortedLeft, ?SortedRight, ?Merged)
% Merges two sorted lists into a single sorted list.
merge(_Gt, _Lte, [], L, L).
merge(_Gt, _Lte, L, [], L).
merge(Gt, Lte, [X|Xs], [Y|Ys], [X|Zs]) :-
	call(Lte, X, Y),
    merge(Gt, Lte, Xs, [Y|Ys], Zs).
merge(Gt, Lte, [X|Xs], [Y|Ys], [Y|Zs]) :-
	call(Gt, X, Y),
    merge(Gt, Lte, [X|Xs], Ys, Zs).

merge_lon(X,Y,Z) :-
	merge(\A^B^(#A #> #B), \A^B^(#A #=< #B), X, Y, Z).

%% ?- mergesort(merge_lon,X,Y).


%% §6.7

%% Todo: general structure inequality constraints a la constrained.pl
%% https://github.com/bakaq/constrained.pl/issues/17

%% Exercises 6

% 6.1
weekdays(['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday']).

% 6.2
%%
%% IDC, it's better to start at zero.

%% Should either constrain number to within a month range (or), a
%% member_c constraint.
date_of(N,D) :- weekdays(L), date_of(N,L,D).

date_of(N,[],D) :- #N #>= 0, weekdays(L), date_of(N,L,D).
date_of(N,[D|_],D) :- #N #= 0.
date_of(N,[_|Ds],D) :- #N #= #M + 1, #M #>= 0, date_of(M,Ds,D).


% 6.3

% rem(?Term, ?Term, ?Term)
%
% Relates Term1, a list containing Term1, and the same list without
% that first occurrence.
%
%% rem(_,[],[]). %% Optionally relate lists that never had it as a member.
rem(X,[Y|L0],L) :- if_(X = Y, L0 = L, rem(X,L0,L)).

%% 2-clause dif/2 based implementation.
%% rem(X,[X|L],L).
%% rem(X,[Y|L0],[Y|L]) :- dif(X,Y), rem(X,L0,L).

% 6.4

last_elem(X,[X]).
last_elem(X,[_,Y1|L]) :- last_elem(X,[Y1|L]).


% 6.5

%% lset_difference
%
% List2 \ List1 = List3
%
% TODO: check sethood (as we recur down the lists).
% TODO: literally a difference list--so use DCG?

lset_difference([],_,[]).
lset_difference([X|Xs],Rem,Out) :-
	lset_difference(Xs,Rem,Out0),
	if_(memberd_t(X,Rem),
		Out0 = Out,
		Out = [X|Out0]).

%% ?- length(Ls,_), append(X,Y,Ls), lset_difference(Y,X,"abcd").

lset_intersect(L0,L1,L) :-
	lset_difference(L0,L1,L2),
	lset_difference(L1,L2,L).

% 6.6

%% lset_union(L0,L1,L) :-
%% 	lset_difference(L0,L1,L2),
%% 	append(L2,L1,L).

%% Fuses the append and set_diff
%%
lset_union([],L,L).
lset_union([X|L1],L0,[X|L]) :-
	lset_union(L1,L0,L2),
	tfilter(dif(X),L2,L).

%% 6.7

%% Direct
nrev([],[]).
nrev([H|T],[RH|RT]) :- nrev(T,AT), append(AT,[H],[RH|RT]).

%% w/Acc

rev_acc([H|T], R) :- foldl(cons, T, [H], R).
cons(E,Acc,[E|Acc]).

%% 6.8

%% 6.10

stock([item(1023,99.95),item(1024,29.95),item(1021,46.75)]).

stock_price(Num,Price) :-
	stock(L),
	stock_price(Num,L,Price).
stock_price(Num,[item(Num,Price)|_],Price).
stock_price(Num,[item(Num2,_)|R],Price) :- #Num #\= #Num2, stock_price(Num,R,Price).

%% 6.11

comp_item(item(Num1,_),item(Num1,_),0).
comp_item(item(Num1,_),item(Num2,_),-1) :- #Num1 #> #Num2.
comp_item(item(Num1,_),item(Num2,_),-1) :- #Num1 #< #Num2.

%% ?- comp_item(item(cat,_),item(dog,_),-1).
%%    error(type_error(integer,cat),must_be/2).

%% Either pass the comparator as a parameter, or maybe do something
%% like term_expansion to generate at compile time a specialized
%% version of mergesort that already sorts on item number.


%% §7.2
%%

%% My version of his abcdefg cut/1 program is in 'abcdefgh.pl'.

%% §7.3

%% n_queens http://metalevel.at/nqueens/nqueens.pl

n_queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	#Q0 #\= #Q,
    abs(Q0 - Q) #\= #D0,
    #D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).


%% Exercises 7

%% 7.1

%% Not needed but improves the version in the book.
% close_numbers(N,M) :- abs(#N - #M) #=< 1.

%% replace(?InputList ?OutputList ?Substituendum ?Substituend)//2
%%
%% Replaces one occurrence of Substituendum for Substituend InputList
%%
%% To succeed, Substituendum must be a member of InputList.
%% And X must be different from Y (for use case in one_char_away)
replace(X,Y) --> [X], { dif(X,Y) }, [Y].
replace(X,Y) --> [Z], { dif(X,Y), dif(Z,X) }, [Z], replace(X,Y).

%% one_char_away(?List, ?List).
%%
%% Holds when the two lists of equal length differ in a single
%% position.
%%
%% (Needs to be indexed a la indexing dif/2 to control backtracking,
%% That way each word is only considered a once for each.)
one_char_away(String1,String2) :- select(_,String1,String2).
one_char_away(String1,String2) :- select(_,String2,String1).
one_char_away(String1,String2) :- replace(_,_,String1,String2).

%% Current behavior (bad that it says yes 3x? There are 3 chars we can remove.)
%% ?- one_char_away("ddd","dd").
%%    true
%% ;  true
%% ;  true
%% ;  false.

%% near_miss(?Word)/2
%
% A word is a near miss if it exactly one char away
% away from a word in our database.
near_miss(Word) :-
	atom_chars(Word,String1),
	word(Word2),
	atom_chars(Word2,String2),
	one_char_away(String1,String2).

word(ducal).
word(ducat).
word(duchess).
word(duchesse).
word(duchy).
word(duck).
word(duckling).
word(duct).
word(ductile).
word(dud).
word(dude).
word(due).
word(duel).
word(duenna).
word(duet).
word(duff).
word(duffer).
word(duffle).

%% 7.3

%% 7.4

%% ?- findall(X,near_miss(duch,X),Solutions).
%%    Solutions = [duchy,duck,duct].

% Chapter 8

%% §8.2

%% word//1
%
% Describes a non-empty sequence of non-space characters.
word([C|L]) --> [C], {dif(C,' ')}, word_(L).

%% word_//1
%
% Describes a sequence of non-space characters.
word_([]) --> "".
word_([C|L]) --> [C], {dif(C,' ')}, word_(L).

%% sequence//1
%
% Describes a sequence of words, each separated by any number of spaces.
% Sequence possibly has leading or trailing spaces.
sequence([]) --> [].
sequence([L]) --> word(L).
sequence(L) --> " ", sequence(L).
sequence([L0|L]) --> word(L0), " ", sequence(L).

%% ?- phrase(sequence(L),"  ab  cd a d abra").
%%    L = ["ab","cd","a","d","abra"]
%% ;  false.
%% ?- phrase(sequence(L),"ab  cd a d abra  ").
%%    L = ["ab","cd","a","d","abra"]
%% ;  false.
%% ?- phrase(sequence(L),"").
%%    L = []
%% ;  false.
%% ?- phrase(sequence(L)," ").
%%    L = []
%% ;  false.
%% ?- phrase(sequence(L),"a").
%%    L = ["a"]
%% ;  false.
%% ?- length(Y,_), phrase(sequence(L),Y).
%%    Y = [], L = []
%% ;  Y = [_A], L = [[_A]], dif:dif(_A,' ')
%% ;  Y = " ", L = []
%% ;  Y = [_A,_B], L = [[_A,_B]], dif:dif(_A,' '), dif:dif(_B,' ')
%% ;  Y = [' ',_A], L = [[_A]], dif:dif(_A,' ')
%% ;  Y = "  ", L = []
%% ;  Y = [_A|" "], L = [[_A]], dif:dif(_A,' ')
%% ;  Y = [_A,_B,_C], L = [[_A,_B,_C]], dif:dif(_A,' '), dif:dif(_B,' '), dif:dif(_C,' ')
%% ;  Y = [' ',_A,_B], L = [[_A,_B]], dif:dif(_A,' '), dif:dif(_B,' ')
%% ;  Y = [' ',' ',_A], L = [[_A]], dif:dif(_A,' ')
%% ;  Y = "   ", L = []
%% ;  Y = [' ',_A|" "], L = [[_A]], dif:dif(_A,' ')
%% ;  Y = [_A,' ',_B], L = [[_A],[_B]], dif:dif(_A,' '), dif:dif(_B,' ')
%% ;  Y = [_A|"  "], L = [[_A]], dif:dif(_A,' ')error('$interrupt_thrown',get_single_char/1).
%% ?-

%% §8.3

%% Exercises 8

%% 8.1

%% Should have a constraint for upcased characters. I don't know that
%% this is technically possible, because I don't know enough about the
%% unicode standard to know if there's a way to tell if what you have
%% is an upcaseable character. I don't want to enumerate Latin-1.

%% 8.2

line([]) --> "\n".
line([C|L]) --> [C], {dif(C,'\n')}, line(L).

address([]) --> [].
address([L0|L]) --> line(L0), address(L).

%% ?- use_module(library(files)).
%% ?- phrase_from_file(address(L),"/Users/jhemann/temp/address.txt").

%% Chapter 9

connected(birmingham,nuneaton, 25).
connected(nuneaton,leicester, 18).
connected(birmingham, coventry, 20).
connected(coventry, rugby, 8).
connected(coventry, leamington, 8).
connected(birmingham, leamington, 21).
connected(birmingham, stratford, 25).
connected(stratford, leamington, 15).
connected(nuneaton, rugby, 12).
connected(coventry, nuneaton, 10).

connected2(X, Y, D) :- connected(X, Y, D).
connected2(X, Y, D) :- connected(Y, X, D).

%% We need absento _constraint_ to do dismembership properly.
absento_list(X,L) :- maplist(dif(X),L).

next_node(Current, Next, Path) :-
	connected2(Current, Next, _),
	absento_list(Next, Path).

%% I don't know what sense it makes to discuss these predicates as DFS
%% or BFS, because the "first"ness relies on the order of the return
%% of the answers, which is operational behavior.
%%
%% I feel a predicate's name should reflect its declarative semantics.
%%
%% You might want a list of the answers in BFS order.


depth_first(Target, Target, _, [Target]).
depth_first(Start, Target, Visited, [Start|Path]) :-
	next_node(Start, Next_node, Visited),
	depth_first(Next_node, Target, [Next_node|Visited], Path).

%% 9.4

breadth_first(Start, Target, Path) :-
	breadth_first_([[Start]], Target, Path).

%% breadth_first_/3
%
%
breadth_first_([[Target|Path]|_], Target, [Target|Path]).
breadth_first_([[Current|Trail]|OtherPaths], Target, Path) :-
	findall([Next,Current|Trail], next_node(Current,Next,Trail), NewPaths),
	append(OtherPaths,NewPaths,AllPaths),
	breadth_first_(AllPaths, Target, Path).

%% 9.5

path_distance([_], 0).
path_distance([A,B|Rest], D) :-
	#D #= #D1 + #D2,
	path_distance([B|Rest],D2),
	connected2(A,B,D1).


%% Exercises 9

%% 9.3

%% ?- depth_first(birmingham,rugby, X, Path).
%%    X = [], Path = [birmingham,nuneaton,rugby]
%% ;  ... .

%% 9.5

%% TODO: Use some setof to calculate all of the nodes reached by BFS in getting to the final point.

%% 9.6

%% ?- breadth_first(birmingham,rugby, Path).
%%    Path = [rugby,nuneaton,birmingham]
%% ;  ... .
%% ?-

%% 9.8


gt_route(P0,P1) :-
	#D0 #> #D1,
	path_distance(P0,D0),
	path_distance(P1,D1).

lte_route(P0,P1) :-
	#D0 #=< #D1,
	path_distance(P0,D0),
	path_distance(P1,D1).

merge_paths(X,Y,Z) :- merge(gt_route, lte_route, X, Y, Z).

%% best_first/3
%
%
best_first(Start, Goal, Path) :-
	best_first_([[Start]], Goal, Path).

%% best_first_/3
%
%
best_first_([[Goal|Path]|_], Goal, [Goal|Path]).
best_first_([[Current|Trail]|OtherPaths], Goal, Path) :-
	findall([Next,Current|Trail], next_node(Current,Next,Trail), NewPaths),
	mergesort(merge_paths,NewPaths,SortedNewPaths),
	append(OtherPaths,SortedNewPaths,AllPaths),
	best_first_(AllPaths, Goal, Path).


%% Chapter 10

%% 10.3

%% Implemented using DCGs

sentence(sentence(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(np(NP2))	--> noun_phrase2(NP2).
noun_phrase(np(D,NP2))	--> determiner(D), noun_phrase2(NP2).

noun_phrase2(np2(N))		--> noun(N).
noun_phrase2(np2(A,NP2))	--> adjective(A), noun_phrase2(NP2).

verb_phrase(vp(V))		--> verb(V).
verb_phrase(vp(V,NP))	--> verb(V), noun_phrase(NP).
verb_phrase(vp(V,P))	--> verb(V), prepositional_phrase(P).

prepositional_phrase(prep(P,NP)) --> preposition(P), noun_phrase(NP).

determiner(determiner(the)) --> [the].

noun(noun(boy))		--> [boy].
noun(noun(deck))	--> [deck].

adjective(adjective(burning)) --> [burning].

verb(verb(stood)) --> [stood].

preposition(preposition(on)) --> [on].

%% ?- phrase(sentence(X), [the,boy,stood,on,the,burning,deck]).
%%    X = sentence(np(determiner(the),np2(noun(boy))),vp(verb(stood),prep(preposition(on),np(determiner(the),np2(adjective(burning),np2(noun(deck)))))))

%% 10.4

%% sequence//1
%
% Describes a sequence of atoms, each separated by any number of spaces.
% Sequence possibly has leading or trailing spaces.
sequence_atoms([])		--> [].
sequence_atoms([A])		--> word(L), { atom_chars(A, L) }.
sequence_atoms(L)		--> " ", sequence_atoms(L).
sequence_atoms([A|L])	--> word(L0), { atom_chars(A,L0) }, " ", sequence_atoms(L).

%% query//1
%
%
%
query(q(Sel,F,W)) --> sql_select(Sel), from(F), where(W).

%% sql_select//1
%
% Named `sql_select` to avoid conflict w/`select` in library(lists)
sql_select(select(Names)) --> [select], colnames(Names).

%% colnames//1
%
colnames([Name])		--> colname(Name).
colnames([Name|Names])	--> colname(Name), colnames(Names).

from(table(T)) --> [from], tablename(T).

where_clause([Colname,Op,Value]) --> colname(Colname), sql_op(Op), value(Value).

%% where//1
%
% describes sql `where` clause w/a possibly empty sequence of conditions
where(where([]))				--> [].
where(where([Clause]))			--> [where], where_clause(Clause).
where(where([Clause|Clauses]))	--> [where], where_clause(Clause), where_(Clauses).

%% where_//1
%
% Describes the rest of a non-empty sequence of conditions in an sql `where` clause.
where_([Clause])			--> [and], where_clause(Clause).
where_([Clause|Clauses])	--> [and], where_clause(Clause), where_(Clauses).

sql_op(equal) --> [equal].

value(V) --> [V].

colname(C) --> { colnames(ColList), mem(C,ColList,_) }, [C].
tablename(emp) --> [emp].

%% ?- phrase(sequence_atoms(S),"select empno empname from emp"), phrase(query(Q),S).
%%    S = [select,empno,empname,from,emp], Q = q(select([empno,empname]),(table emp))
%% ;  false.

%% 10.5

colnames([empno,empname,job]).
table(emp, [1,smith,clerk]).
table(emp, [2,jones,accountant]).
table(emp, [3,peters,lawyer]).
table(emp, [4,king,director]).

%%
%
%
match_select(Tname,SelectColIdxs, SelectColVals) :-
	table(Tname,Record),
	maplist(Record+\Idx^Val^nth1(Idx,Record,Val),SelectColIdxs,SelectColVals).

%% Matches a parsed SQL statement against a database of data.
%
% TODO implement where clause matching, refactor.
do_query(q(select(SelectCols), table(Tname), where(_)), Answer) :-
	colnames(AllCols),
	maplist(AllCols+\Col^Idx^nth1(Idx,AllCols,Col), SelectCols, SelectColIdxs),
	findall(SelectColVals, match_select(Tname,SelectColIdxs, SelectColVals), Answer).

%% ?- phrase(sequence_atoms(S),"select empno empname from emp"), phrase(query(Q),S), do_query(Q,Answer).
%%    S = [select,empno,empname,from,emp], Q = q(select([empno,empname]),table(emp)), Answer = [[1,smith],[2,jones],[3,peters],[4,king]]

%% ?- phrase(sequence_atoms(S),"select empno empname from emp where job equal lawyer and empname equal peters"), phrase(query(Q),S).
%%    S = [select,empno,empname,from,emp,where,job,equal,lawyer,and,empname,equal,peters],
%%    Q = q(select([empno,empname]),table(emp),where([[job,equal,lawyer],[empname,equal,peters]]))

%% Exercises 10.

%% 10.2

%% Note this approach will not work as is for emplnum, b/c parsing
%% presumes strings of characters.

%% 10.3

%% TODO: Implement where clause matching
%%
%% TODO: A more sophisticated way to implement
%% describe the where clauses
%%
%% TODO: Add more operators besides = "equal"
%%
%% TODO: More higher-order way of working w/clauses.

%% Chapter 11

%% §11.2

%% Chapter 12

%% §12.5.3

with_certainty(has(george,aching_limbs), 90).
with_certainty(has_property(george,sweaty), 70).
with_certainty(has(george,fever), 60).
rule_with_certainty(:-(has(X,flu),','(has(X,temperature),has(X,aching_limbs))),20).
rule_with_certainty(:-(has(X,temperature),','(has_property(X,sweaty),has(X,fever))),20).

deduce_with_certainty(F,C) :-
	backward_with_certainty(F,C).

backward_with_certainty(F,C) :-
	#C #> 0,
	#C0 #=< 100,
	#C0 #>= #C,
	with_certainty(F,C0).
backward_with_certainty(H,C) :-
	#C0 #> 0,
	#C1 #> 0,
	#C #=< 100,
	#C #=< C0 * C1,
	rule_with_certainty(:-(H,Bod), C0),
	backward_with_certainty(Bod,C1).
backward_with_certainty(','(G1,G2),C) :-
	#C #=< 100,
	#C1 #> 0,
	#C2 #> 0,
	#C #=< min(#C1,#C2),
	backward_with_certainty(G1,C1),
	backward_with_certainty(G2,C2).

%% ?- deduce_with_certainty(has_property(X,Y),_).
%%    X = george, Y = sweaty, clpz:(_A in 1..70)
%% ;  false.
%% ?- deduce_with_certainty(has(X,Y),_).
%%    X = george, Y = aching_limbs, clpz:(_A in 1..90)
%% ;  X = george, Y = fever, clpz:(_A in 1..60) ;; *************************
%% ;  X = george, Y = flu, clpz:(_A in 1..90), clpz:(#_B#>= #_A), clpz:(20* #_A#= #_C), clpz:(_B in 1..90), clpz:(#_D#>= #_B), clpz:(#_E#>= #_B), clpz:(#_B#=min(#_E,#_D)), clpz:(_D in 1..90), clpz:(_E in 1..100), clpz:(#_F#>= #_E), clpz:(_F in 20..1200), clpz:(20* #_G#= #_F), clpz:(_G in 1..60), clpz:(#_H#>= #_G), clpz:(_H in 1..60), clpz:(#_I#>= #_H), clpz:(#_J#>= #_H), clpz:(#_H#=min(#_J,#_I)), clpz:(_I in 1..60), clpz:(_J in 1..70), clpz:(_C in 20..1800), clpz:(#_C#>= #_K), clpz:(_K in inf..100)
%% ;  X = george, Y = temperature, clpz:(_A in 1..60), clpz:(#_B#>= #_A), clpz:(20* #_A#= #_C), clpz:(_B in 1..60), clpz:(#_D#>= #_B), clpz:(#_E#>= #_B), clpz:(#_B#=min(#_E,#_D)), clpz:(_D in 1..60), clpz:(_E in 1..70), clpz:(_C in 20..1200), clpz:(#_C#>= #_F), clpz:(_F in inf..100)
%% ;  false.
%% ?-

%% Question: It seems like we are reifying irrelevant constraints here. Is that desired?

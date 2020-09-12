:-module(recipes, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  %,recipe/2
		  ,replace/4
		  ,break_eggs/2
		  ,whisk_eggs/2
		  ,heat_oil/2
		  ,fry_eggs/2
		  ,season/2
		  ,replace/4
		  ]).

:-use_module(configuration).

/* <module> Experiment file for learning cooking recipes.

Usage instructions
------------------

1. Set configuration option max_invented/1 to 3 with a
set_configuration_option/2 directive:

==
auxiliaries:set_configuration_option(max_invented, [3]).
==

2. Run a learning query with a dynamic learning predicate to perform
predicate invention:

==
?- learn_dynamic(recipe/2).
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$2'(A,B):-heat_oil(A,C),'$3'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$3'(C,B).
'$3'(A,B):-fry_eggs(A,C),season(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
true.
==


Dynamically setting configuration options
-----------------------------------------

This experiment file demonstrates the use of the auxiliary predicate
set_configuration_option/2 to dynamically manipulate configuration
options.

The configuration option max_invented/1 is set dynamically using
set_configuration_option/2 as a directive (at the start of the source
code, below):

==
auxiliaries:set_configuration_option(max_invented, [3]).
==

Note that setting a configuration option dynamically using
set_configuration_option/2 will _not_ reset the configuration option
after any learning attempt. This means that subsequent learning attempts
will retain the value of the dynamically changed option. This will
usually not be what is expected and may well cause some confusion.

For the time being the only sure-fire way to reset a configuration
option to its original value is to edit the value of that option in the
configuration file and then reload the configuration file with make/0.
*/

:- auxiliaries:set_configuration_option(max_invented, [3]).

% Tells list_learning_results/0 to use the right learning predicate.
configuration:learning_predicate(learn_dynamic/1).

background_knowledge(recipe/2,[break_eggs/2
			      ,whisk_eggs/2
			      ,heat_oil/2
			      ,fry_eggs/2
			      ,season/2
			      ,replace/4
			      ]).

metarules(recipe/2,[chain]).

% Replace clause above with this one to obtain more specific recipes
% only working for making omelette
% metarules(recipe/2,[chain_abduce_y]).

positive_example(recipe/2,E):-
	member(E, [recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette])
		  ]).

negative_example(recipe/2,_):-
	fail.

break_eggs(Xs,Ys):-
	replace([eggs],Xs,[egg_whites,egg_yolks],Ys).
whisk_eggs(Xs,Ys):-
	replace([egg_whisk,egg_whites,egg_yolks],Xs,[whisked_eggs],Ys).
heat_oil(Xs,Ys):-
	replace([frying_pan,olive_oil],Xs,[frying_oil],Ys).
fry_eggs(Xs,Ys):-
	replace([frying_oil,whisked_eggs],Xs,[frying_eggs],Ys).
season(Xs,Ys):-
	replace([frying_eggs,pepper,salt],Xs,[omelette],Ys).


%!	replace(+Set1,+Set2,+Set3,+Set4) is det.
%
%	Replace Set1 in Set3 with Set2 to make Set4.
%
%	Set1, Set2, Set3 and Set4 are ordered sets, i.e. list sorted to
%	the standard order of terms and without any duplicates.
%
%	Set4 is Set3 subtracting Set1 and adding Set2. Or, more
%	formally:
%	==
%	Set4 = (Set3 \ Set1) U Set2
%	==
%
%	@tbd Makes no attempt to test whether any of its arguments is an
%	ordered set.
%
replace(Xs,Is,Ys,Os):-
	ground(Xs)
	,ground(Is)
	,ground(Ys)
	,ord_subset(Xs,Is)
	,ord_subtract(Is,Xs,Zs_)
	,ord_union(Ys,Zs_,Os).


% Target theory for omelette
% Thelma learns a better one with a bit of invention.
recipe_(As,Fs):-
	break_eggs(As,Bs)
	,whisk_eggs(Bs,Cs)
	,heat_oil(Cs,Ds)
	,fry_eggs(Ds,Es)
	,season(Es,Fs).

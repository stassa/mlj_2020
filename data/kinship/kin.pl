:-module(kin, [background_knowledge/2
	      ,metarules/2
	      ,positive_example/2
	      ,negative_example/2
	      ,ancestor/2
	      ,grandparent/2
	      ,grandfather/2
	      ,grandmother/2
	      ,parent/2
	      ,husband/2
	      ,wife/2
	      ,child/2
	      ,son/2
	      ,daughter/2
	      ,father/2
	      ,mother/2
	      ,male/1
	      ,female/1
	      ]).

background_knowledge(kin/2, [ancestor/2
                            ,grandparent/2
                            ,grandfather/2
                            ,grandmother/2
                            ,parent/2
                            ,husband/2
                            ,wife/2
                            ,child/2
                            ,son/2
                            ,daughter/2
                            ,father/2
                            ,mother/2
                            ,male/1
                            ,female/1]).

metarules(kin/2,[chain,tailrec,switch,precon]).

positive_example(kin/2,kin(A,B)):-
	kin(A,B)
	,A \= B.

negative_example_(kin/2,kin(A,A)):-
	kin(A,A).

negative_example(kin/2,kin(A,B)):-
	individual(A)
	,individual(B)
	,A \= B
	,\+ once(kin(A,B)).

individual(A):-
	male(A).
individual(A):-
	female(A).

%/* Target theory
kin(A,B):- ancestor(A,B).
kin(A,B):- ancestor(B,A).
kin(A,B):- ancestor(C,A), ancestor(C,B).
kin(A,B):- husband(A,B).
kin(A,B):- wife(A,B).
%kin(A,B):- child(A,B).
%*/

% Background knowledge.

ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).

grandparent(X,Y):-
	grandfather(X,Y).
grandparent(X,Y):-
	grandmother(X,Y).

grandfather(A,B):-
	father(A,C)
	,parent(C,B).

grandmother(A,B):-
	mother(A,C)
	,parent(C,B).

parent(X, Y):-
	father(X,Y).
parent(X, Y):-
	mother(X,Y).

husband(X,Y):-
	father(X,Z)
	,mother(Y,Z).

wife(X,Y):-
	mother(X,Z)
	,father(Y,Z).

child(X,Y):-
	parent(Y,X).

son(X,Y):-
	male(X)
	,child(X,Y).

daughter(X,Y):-
	female(X)
	,child(X,Y).


father(stathis, kostas).
father(stefanos, dora).
father(stefanos,akis).
father(kostas, stassa).
father(akis,kostis).
father(vassilis,nikolas).
father(vassilis,alexandros).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(paraskevi, akis).
mother(dora, stassa).
mother(efi,kostis).
mother(georgia, nikolas).
mother(georgia, alexandros).

male(stathis).
male(stefanos).
male(kostas).
male(akis).
male(kostis).
male(vassilis).
male(nikolas).
male(alexandros).

female(dora).
female(stassa).
female(alexandra).
female(paraskevi).
female(efi).
female(georgia).


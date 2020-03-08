metagol:max_clauses(40).
metagol:min_clauses(1).
metagol:max_inv_preds(0).

body_pred(parent/2).

metarule([A,B],[A,C,D],[[B,C,E],[A,E,D]]).
metarule([A,B],[A,C,D],[[B,C,D]]).

pos(ancestor/2,ancestor(alexandra,kostas)).
pos(ancestor/2,ancestor(alexandra,stassa)).
pos(ancestor/2,ancestor(dora,stassa)).
pos(ancestor/2,ancestor(kostas,stassa)).
pos(ancestor/2,ancestor(paraskevi,dora)).
pos(ancestor/2,ancestor(paraskevi,stassa)).
pos(ancestor/2,ancestor(stathis,kostas)).
pos(ancestor/2,ancestor(stathis,stassa)).
pos(ancestor/2,ancestor(stefanos,dora)).
pos(ancestor/2,ancestor(stefanos,stassa)).

neg(ancestor/2,ancestor(dora,paraskevi)).
neg(ancestor/2,ancestor(dora,stefanos)).
neg(ancestor/2,ancestor(kostas,alexandra)).
neg(ancestor/2,ancestor(kostas,stathis)).
neg(ancestor/2,ancestor(stassa,alexandra)).
neg(ancestor/2,ancestor(stassa,dora)).
neg(ancestor/2,ancestor(stassa,kostas)).
neg(ancestor/2,ancestor(stassa,paraskevi)).
neg(ancestor/2,ancestor(stassa,stathis)).
neg(ancestor/2,ancestor(stassa,stefanos)).

parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).

mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).


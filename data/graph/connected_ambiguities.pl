:-module(connected_ambiguities, [background_knowledge/2
				,metarules/2
				,positive_example/2
				,negative_example/2
				,ancestor/2
				,parent/2
				,child/2
				,blue_child/2
				,red_child/2
				,blue_parent/2
				,red_parent/2
				,blue/1
				,red/1
				]).

/** <module> Coloured graph dataset with ambiguous examples.

We define an ambiguous example as an atom of a target predicate that is
included both in the positive and (negated) in the negative examples.

This dataset represents an acyclic graph and is constructed such that
paths through the graph starting at nodes: {a,c,e} are labelled as both
positives and negatives, therefore constituting "ambiguities" according
to our definition above.

The purpose of the dataset is to test the ability of Louise and Metagol
to learn correct hypotheses in the presence of ambiguous data, as we
define it above.

*/


background_knowledge(connected/2, [ancestor/2
				  ,parent/2
				  ,child/2
				  ,blue_child/2
				  ,red_child/2
				  ,blue_parent/2
				  ,red_parent/2
				  ,blue/1
				  ,red/1
				  ]).

%metarules(connected/2,[chain,tailrec,switch,precon]).
metarules(connected/2,[identity,inverse,chain,precon]).
%metarules(connected/2,[chain,tailrec,precon]).

positive_example(connected/2,connected(A,B)):-
	connected(A,B)
	,\+ cycle(A,B).

negative_example_(connected/2,_):-
% For testing!
	false.
negative_example(connected/2,connected(A,B)):-
	node(A)
	,node(B)
	,\+ once(connected(A,B))
	,\+ cycle(A,B).
negative_example(connected/2,connected(A,B)):-
% Positive examples of paths starting at nodes a, d and f are also
% labelled as negative.
	member(A, [a,c,e])
	,connected(A,B).


%!	cycle(?Node,?Node) is nondet.
%
%	A node with an edge or path connecting it to itself.
%
cycle(A,A):-
	node(A).


%!	node(?Node) is nondet.
%
%	A coloured Node in a graph.
%
node(A):-
	blue(A).
node(A):-
	red(A).


%!	connected(?Node1,?Node2) is nondet.
%
%	True when Node2 can be reached via a path starting at Node1.
%
%	Target theory for connected/2 MIL problem, used to generate
%	positive and negative examples.
%
connected(A,B):- ancestor(A,B).
connected(A,B):- ancestor(B,A).
connected(A,B):- ancestor(C,A), ancestor(C,B).
connected(A,B):- ancestor(A,C), ancestor(B,C).


%========== Background knowledge. ====================

%!	ancestor(?Ancestor,?Descendant) is nondet.
%
%	Relates an Ancestor node to each of its Descendant nodes.
%
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).


%!	parent(?Parent,?Child) is nondet.
%
%	Relates a Parent node to each of its Child nodes.
%
parent(X, Y):-
	blue_parent(X,Y).
parent(X, Y):-
	red_parent(X,Y).


%!	child(?Child, ?Parent) is nondet.
%
%	Relates a Child node to each of its Parent nodes.
%
child(X,Y):-
	parent(Y,X).


%!	blue_child(?Child, ?Parent) is nondet.
%
%	Relates a blue Child node to each of its Parent nodes.
%
blue_child(X,Y):-
	blue(X)
	,child(X,Y).


%!	red_child(?Child, ?Parent) is nondet.
%
%	Relates a red Child node to each of its Parent nodes.
%
red_child(X,Y):-
	red(X)
	,child(X,Y).


%!	blue_parent(?Parent,?Child) is nondet.
%
%	Relates a blue Parent node to each of its Child nodes.
%
blue_parent(a,c).
blue_parent(a,n).
blue_parent(b,i).
blue_parent(b,d).
blue_parent(c,j).
blue_parent(d,e).
blue_parent(f,g).
blue_parent(f,h).


%!	red_parent(?Parent,?Child) is nondet.
%
%	Relates a red Parent node to each of its Child nodes.
%
red_parent(k,c).
red_parent(k,n).
red_parent(l,i).
red_parent(l,d).
red_parent(i,j).
red_parent(m,e).
red_parent(n,g).
red_parent(n,h).


%!	blue(?Node) is nondet.
%
%	A Node coloured blue.
%
blue(a).
blue(b).
blue(c).
blue(d).
blue(e).
blue(f).
blue(g).
blue(h).


%!	red(?Node) is nondet.
%
%	A Node coloured red.
%
red(i).
red(j).
red(k).
red(l).
red(m).
red(n).

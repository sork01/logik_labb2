:- style_check(-singleton).

% [[s0, [r]], [s1, [q]], [s2, []], [s3, []]].
% \________/  \____________________________/
%    Head                  Tail
%     |
% [[s0, [r]]
%    |    \
%  Start  Stuff

% contains/3    contains(+List, +Key, +Value)
% 
% Determines whether or not Value exists in the adjacency
% list contained in List at index Key. True if the value
% is contained in that list, false otherwise.
contains([[Start, Stuff]|Tail], Start, X):-
    memberchk(X, Stuff), !.

contains([_|Tail], Start, X):-
    contains(Tail, Start, X).

% verify/1    verify(+Filename)
% 
% True if Filename contains a valid system description and
% a statement in limited CTL that is true for that system,
% false otherwise.
verify(Filename):-
    see(Filename),
    read(Nodes),
    read(Truths),
    read(Start),
    read(Formula),
    validate(Nodes, Truths, Start, [], Formula), !,
    seen.


% validate/5    validate(+Nodes, +Truths, +Position, +Visited, +Statement)
% 
% True if Statement holds on the system defined by Nodes
% and Truths starting from Position and having a list of
% previously visited nodes Visited.
% 
% Nodes is a list of adjacency lists in the format:
% [ [node, [neighbor, neighbor, ..., neighbor]], [node ...] ] 
% 
% Truths is a map of states true to certain nodes:
% [ [node, [truth, truth, ..., truth]], [node ...] ]
% 
% Position is one of the node indices from Nodes, visited is
% a list of node indices from Nodes that have already been visited,
% and Statement is a logical statement in limited CTL of the form:
% af(eg(not(q)))

% EF Phi
% EF_1
validate(Nodes, Truths, Start, Visited, ef(X)):-
    \+ memberchk(Start, Visited),
    validate(Nodes, Truths, Start, [], X).

% EF_2
validate(Nodes, Truths, Start, Visited, ef(X)):-
    \+ memberchk(Start, Visited),
    member([Start|[Neighbors]], Nodes),
    !,
    append(Visited, [Start], NewVisited),
    validate_one(Nodes, Truths, Neighbors, NewVisited, ef(X)).

% AF Phi
% AF_1
validate(Nodes, Truths, Start, Visited, af(X)):-
    \+ memberchk(Start, Visited),
    validate(Nodes, Truths, Start, [], X).

% AF_2
validate(Nodes, Truths, Start, Visited, af(X)):-
    \+ memberchk(Start, Visited),
    member([Start|[Neighbors]], Nodes),
    !,
    append(Visited, [Start], NewVisited),
    validate_all(Nodes, Truths, Neighbors, NewVisited, af(X)).

% EG Phi
% EG_1
validate(Nodes, Truths, Start, Visited, eg(X)):-
    memberchk(Start, Visited).

% EG_2
validate(Nodes, Truths, Start, Visited, eg(X)):-
    \+ memberchk(Start, Visited),
    validate(Nodes, Truths, Start, [], X),
    member([Start|[Neighbors]], Nodes),
    !,
    append(Visited, [Start], NewVisited),
    validate_one(Nodes, Truths, Neighbors, NewVisited, eg(X)).

% AG Phi
% AG_1
validate(Nodes, Truths, Start, Visited, ag(X)):-
    memberchk(Start, Visited).

% AG_2
validate(Nodes, Truths, Start, Visited, ag(X)):-
    \+ memberchk(Start, Visited),
    validate(Nodes, Truths, Start, [], X),
    member([Start|[Neighbors]], Nodes),
    !,
    append(Visited, [Start], NewVisited),
    validate_all(Nodes, Truths, Neighbors, NewVisited, ag(X)).

% EX Phi
validate(Nodes, Truths, Start, Visited, ex(X)):-
    member([Start|[Neighbors]], Nodes),
    !,
    member(Next, Neighbors),
    validate(Nodes, Truths, Next, [], X),
    !.

% AX Phi
validate(Nodes, Truths, Start, Visited, ax(X)):-
    member([Start|[Neighbors]], Nodes),
    !,
    validate_all(Nodes, Truths, Neighbors, [], X).

% Phi and Psi
validate(Nodes, Truths, Start, Visited, and(X,Y)):-
    validate(Nodes, Truths, Start, [], X),
    validate(Nodes, Truths, Start, [], Y).

% Phi or Psi
validate(Nodes, Truths, Start, Visited, or(X,Y)):-
    validate(Nodes, Truths, Start, [], X);
    validate(Nodes, Truths, Start, [], Y).

% not Phi
validate(Nodes, Truths, Start, Visited, neg(X)):-
    atom(X),
    \+ validate(Nodes, Truths, Start, [], X).

% p
validate(Nodes, Truths, Start, Visited, X):-
    atom(X),
    contains(Truths, Start, X).

% validate_all/5    validate_all(+Nodes, +Truths, +NodeList, +Visited, +Statement)
% 
% True if, for EACH node index in NodeList, Statement holds
% on the system defined by Nodes and Truths starting at said
% node index. False otherwise. See and compare validate/5.
validate_all(Nodes, Truth, [Head|Tail], Visited, X):-
    validate(Nodes, Truth, Head, Visited, X),
    validate_all(Nodes, Truth, Tail, Visited, X).

validate_all(Nodes, Truth, [], Visited, X).

% validate_one/5    validate_one(+Nodes, +Truths, +NodeList, +Visited, +Statement)
% 
% True if, for SOME node index in NodeList, Statement holds
% on the system defined by Nodes and Truths starting at said
% node index. False otherwise. See and compare validate/5.
validate_one(Nodes, Truth, [Head|Tail], Visited, X):-
    validate(Nodes, Truth, Head, Visited, X).

validate_one(Nodes, Truth, [Head|Tail], Visited, X):-
    validate_one(Nodes, Truth, Tail, Visited, X).

validate_one(Nodes, Truth, [], Visited, X):-
    fail.

main:-
    verify('tests/valid441.txt').

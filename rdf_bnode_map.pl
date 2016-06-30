:- module(
  rdf_bnode_map,
  [
    add_bnode_map/3,   % ?BNode, +Term, ?G
    b2t/3,             % +BNode, -Term, ?G
    clear_bnode_map/1, % ?G
    t2b/3              % +Term, -BNode, ?G
  ]
).

/** <module> RDF term: Blank node mapping

Since literals are not allowed to occur in the subject position of RDF
triples, blank nodes need to be associated with them in order to be
able to form propositions that state things *about* literals.

Blank nodes and IRIs should also be mapped onto a single blank node
in simple entailment, in order to ascertain that the original graph
is a proper instance of every materialized graph.

The graph argument is required in blank node maps, since identical
blank node labels that occur in different graphs may or may not denote
different resources.

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(assoc)).
:- use_module(library(default)).
:- use_module(library(semweb/rdf11)).

%! b2t0(+G, -Map) is semidet.
%! t2b0(+G, -Map) is semidet.
% The mapping from blank nodes to RDF terms and the mapping from RDF
% terms to blank nodes.

:- dynamic
    b2t0/2,
    t2b0/2.





%! add_bnode_map(?BNode, +Term, ?G) is det.
% Adds the mapping between a single blank node and a single RDF term.
%
% This is stored in 2 association lists, one for each direction of the
% mapping.
%
% Mappings are graph-local.
%
% If BNode is not instantiated and then it is either:
%   1. Instantiated with the blank node that is currently mapped to Term, or
%   2. Instantiated to a newly created blank node.

add_bnode_map(BNode, Term, G) :-
  defgoal(q_default_graph, G),
  with_mutex(rdf_bnode_map, (
    add_b2t(BNode, Term, G),
    add_t2b(Term, BNode, G)
  )).

add_b2t(BNode, Term, G) :-
  b2t(BNode, Term, G), !.
add_b2t(BNode, Term, G) :-
  b2t(G, OldB2T),
  retractall(b2t0(G, OldB2T)),
  defgoal(qb_bnode, BNode),
  put_assoc(BNode, OldB2T, Term, NewB2T),
  assert(b2t0(G, NewB2T)).

add_t2b(Term, BNode, G) :-
  t2b(Term, BNode, G), !.
add_t2b(Term, BNode, G) :-
  t2b(G, OldT2B),
  retractall(t2b0(G, OldT2B)),
  put_assoc(Term, OldT2B, BNode, NewT2B),
  assert(t2b0(G, NewT2B)).



%! b2t(+BNode, -Term, ?G) is semidet.
% Returns the RDF term to which the given blank node was mapped, if any.

b2t(BNode, Term, G) :-
  defgoal(q_default_graph, G),
  with_mutex(q_bnode_map, (
    b2t(G, Map),
    get_assoc(BNode, Map, Term)
  )).



%! clear_bnode_map(?G) is det.
% Removes the blank node maps for the given graph.

clear_bnode_map(G) :-
  defgoal(q_default_graph, G),
  with_mutex(rdf_bnode_map, (
    retractall(b2t0(G,_)),
    retractall(t2b0(G,_))
  )).



%! t2b(+Term, -BNode, ?G) is semidet.
% Returns the RDF term to which the given blank node was mapped, if any.

t2b(Term, BNode, G) :-
  defgoal(q_default_graph, G),
  with_mutex(rdf_bnode_map, (
    t2b0(G, Map),
    get_assoc(Term, Map, BNode)
  )).



%! term_set_bnode(+Term, -BNode, ?G) is det.
% Either an existing mapping is returned,
%  or a new mapping is created and returned.

% The store contains a blank node that stands for the given resource.
term_set_bnode(Term, BNode, G) :-
  defgoal(q_default_graph, G),
  with_mutex(q_bnode_map, (
    (   t2b0(G, Map),
        get_assoc(Term, Map, BNode)
    ->  true
    ;   % The store does not contain a blank node that stands for the
        % given resource, so a new blank node is created to stand for
        % the given resource and this resource-and-blank-node-pair is
        % added to the B2T and T2B mappings.
        qb_bnode(BNode),
        add_bnode_map(BNode, Term, G)
    )
  )).





% HELPERS

%! b2t(+G, -Map) is det.
% Returns the blank-node-to-term-map for the given graph.
%
% This predicate ensures that the map exists.

b2t(G, Map) :-
  b2t0(G, Map), !.
b2t(G, Map) :-
  empty_assoc(Map),
  assert(b2t0(G, Map)).



%! t2b(+G, -Map) is semidet.
% Returns the term-to-blank-node-mapping for the given graph.
%
% This ensures that the mapping exists.

t2b(G, Map) :-
  t2b0(G, Map), !.
t2b(G, Map) :-
  empty_assoc(Map),
  assert(t2b0(G, Map)).

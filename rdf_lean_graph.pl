:- module(
  rdf_lean_graph,
  [
    rdf_is_lean_graph/1, % +Graph:atom
    rdf_lean_graph/2 % +Graph:atom
                     % -NonLeanTriple:compound
  ]
).

/** <module> RDF Lean graph

An RDF graph is **lean** if it has no instance which is a proper subgraph
of itself.

Non-lean graphs have internal redundancy and express the same content
as their lean subgraphs. For example, the graph

```turtle
ex:a ex:p _:x .
_:y  ex:p _:x .
```

is not lean, but

```turtle
ex:a ex:p _:x .
_:x  ex:p _:x .
```

is lean.

Ground graphs are lean.

Algorithm
---------

The idea behind the algorith is that a non-lean graph must contain
two triples: Generic and Specific such that
the latter is a proper instance of the former.
This means that Specific entails Generic, which is therefore verbose.

@author Wouter Beek
@compat RDF 1.1 Semantics
@version 2014/11, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plRdf(rdf_triples)).
:- use_module(plRdf(term/rdf_instance)).





%! rdf_is_lean_graph(+Lean:atom) is semidet.

rdf_is_lean_graph(Graph):-
 \+ rdf_lean_graph(Graph, _).



%! rdf_lean_graph(+Graph:atom, -NonLeanTriple:compound) is nondet.

rdf_lean_graph(Graph, rdf(S1,P,O1)):-
  rdf_triples(Graph, Triples),
  partition(rdf_is_ground_triple, Triples, Ground, NonGround),
  \+ (
    % Generic. Example 1: `_:y ex:p _:x`.
    member(rdf(S1,P,O1), NonGround),
    % Specific. Example 2: `ex:a ex:p _:x`.
    % This is likelier to be found in ground triples.
    (   member(rdf(S2,P,O2), Ground)
    ;   member(rdf(S2,P,O2), NonGround)
    ),

    % Check whether Specific is a *proper instance* of Generic.
    \+ (S1 == S2, O1 == O2),
    rdf_triple_instance(rdf(S2,P,O2), rdf(S1,P,O1), Map),

    % Check whether the mapping extends to the entire graph.
    forall(
      member(rdf(S3,P3,O3), NonGround),
      (
        rdf_triple_instance(rdf(S3,P3,O3), rdf(S4,P4,O4), Map),
        memberchk(rdf(S4,P4,O4), Triples)
      )
    )
  ).


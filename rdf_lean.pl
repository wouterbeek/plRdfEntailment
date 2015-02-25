:- module(lean, [lean/1]).


/** <module> Lean

Counterexample to the implementation of lean graphs in CPACK rdf-mt.

The algorithm is build around the notion that every non-lean graph contains
 two triples Specific and Generic such that Specific entails Generic.
However, some lean graphs adhere to this notion as well.
The algorithm therefore incorrectly fails on some lean graphs,
 in addition to correcty failing on each non-lean graph.

The following lean graph is a counterexample:

```ntriples
rdf:a rdf:p _:x .
_:y   rdf:p _:x .
rdf:b rdf:p _:x .
```

Substitution [_:x/rdf:a] results in an instance that is not a subgraph:

```ntriples
rdf:a rdf:p rdf:a .
_:y   rdf:p rdf:a .
rdf:b rdf:p rdf:a .
```

The problem here is that the substitutions that need to be made by
 subsumes_term/2 have to be applied consistently to the whole graph
 and not only to the two triples Generic and Specific.
A graph is non-lean if the substitutions made by subsumes_term/2
 can be applied to the whole graph without resulting in a new triple.

@author Wouter Beek
@version 2014/11/15
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- initialization(init).



lean(Graph):-
  findall(
    rdf(S,P,O),
    (
      rdf(S0, P, O0, Graph),
      bnode_to_var(S0, S),
      bnode_to_var(O0, O)
    ),
    Triples
  ),
  partition(ground, Triples, Ground, NonGround),
  \+ (
    member(Gen, NonGround),
    (   member(Spec, Ground)
    ;   member(Spec, NonGround)
    ),
    Gen \== Spec,
    subsumes_term(Gen, Spec),
    format('Counterexample: ~w |= ~w\n', [Spec,Gen])
  ).



% HELPERS

bnode_to_var(BNode, _):-
  rdf_is_bnode(BNode), !.
bnode_to_var(Name, Name).



% INITIALIZATION

init:-
  rdf_bnode(X),
  rdf_bnode(Y),
  rdf_assert(rdf:a, rdf:p, X, test),
  rdf_assert(Y,	rdf:p, X, test),
  rdf_assert(rdf:b, rdf:p, X, test).

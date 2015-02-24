:- module(
  rdf_closure,
  [
    property_path/3, % +Resource1:iri
                     % +Resource1:iri
                     % -Path:list(iri)
    property_path_broader/3
  ]
).

:- use_module(library(apply)).
:- use_module(library(dif)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(lambda_meta)).

:- meta_predicate(closure(3,+,+,-)).
:- meta_predicate(closure0(3,+,+,-,+)).

:- rdf_meta(property_path(r,r,-)).
:- rdf_meta(property_path_broader(r,r,-)).





property_path(X, Y, Path):-
  closure(rdf, X, Y, Path).

closure(G_3, X0, Y, [P|Path]):-
  call(G_3, X0, P, X),
  closure0(G_3, X, Y, Path, [X0,X]).

closure0(_, X, X, [], _).
closure0(G_3, X1, Y, [P|Path], Hist):-
  call(G_3, X1, P, X2),
  maplist(dif(X2), Hist),
  closure0(G_3, X2, Y, Path, [X1|Hist]).


property_path_broader(X, Y, Path):-
  closure(rdf_broader, X, Y, Path).

rdf_broader(X, P, Y):-
  rdf_global_id(skos:broader, P),
  rdf_has(X, P, Y).


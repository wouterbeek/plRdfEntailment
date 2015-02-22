:- module(
  class_distance,
  [
    init_test/0,
    instance_of/3, % +Instance:iri
                   % ?Class:iri
                   % -Path:list(iri)
    most_specific_class/3 % +Resource1:iri
                          % +Resource1:iri
                          % -MostSpecificClass:iri
  ]
).

:- use_module(library(semweb/rdf_db)).

:- use_module(plSet(set_theory)).

:- use_module(plRdf(management/rdf_load_any)).
:- use_module(plRdf(management/rdf_prefixes)).

:- use_module(lodCache(lod_cache_egograph)).

:- rdf_meta(most_specific_class(r,r,-)).
:- rdf_meta(instance_of(r,r,-)).





init_test:-
  assert_cc_prefixes,
  assert_dbpedia_localizations,
  rdf_load_any(
    'http://downloads.dbpedia.org/3.9/en/skos_categories_en.ttl.bz2',
    [graph(categories),if(not_loaded)]
  ).



%! most_specific_class(+Resource1:iri, +Resource2:iri, -Class:iri) is nondet.

most_specific_class(X, Y, D):-
  lod_cache_egograph(X, []),
  lod_cache_egograph(Y, []),
  rdf_has(X, dcterms:subject, XC),
  rdf_has(Y, dcterms:subject, YC),
  rdf_reachable(XC, skos:broader, D),
  rdf_reachable(YC, skos:broader, D),
  \+ (
    rdf_reachable(E, rdfs:subClassOf, D),
    E \== D,
    rdf_reachable(XC, skos:broader, E),
    rdf_reachable(YC, skos:broader, E)
  ).



%! instance_of(+Instance:iri, ?Class:iri, -Path:list(iri)) is nondet.

instance_of(I, C, Path):-
  lod_cache_egograph(I, []),
  rdf_has(I, dcterms:subject, C0),
  rdf_broader(C0, C, Path).

rdf_broader(C, C, []).
rdf_broader(C, E, [C|T]):-
  rdf_has(C, skos:broader, D),
  rdf_broader(D, E, T).


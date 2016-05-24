:- module(
  class_distance,
  [
    init_test/0,
    instance_of/3,        % +I, ?C, -Path
    most_specific_class/3 % +I1, +I2, -C
  ]
).

/** <module> Class distance

@author Wouter Beek
@version 2015/03, 2016/05
*/

:- use_module(library(rdf/rdf_io).
:- use_module(library(semweb/rdf11).

:- rdf_meta
   most_specific_class(r, r, -),
   instance_of(r, r, -).





init_test:-
  assert_cc_prefixes,
  assert_dbpedia_localizations,
  rdf_load_file(
    'http://downloads.dbpedia.org/3.9/en/skos_categories_en.ttl.bz2',
    [graph(categories),if(not_loaded)]
  ).



%! most_specific_class(+Resource1:iri, +Resource2:iri, -Class:iri) is nondet.

most_specific_class(X, Y, D):-
  lod_cache(X, []),
  lod_cache(Y, []),
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
  lod_cache(I, []),
  rdf_has(I, dcterms:subject, C0),
  rdf_broader(C0, C, Path).

rdf_broader(C, C, []).
rdf_broader(C, E, [C|T]):-
  rdf_has(C, skos:broader, D),
  rdf_broader(D, E, T).

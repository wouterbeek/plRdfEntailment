:- module(
  rdf_ent_stat,
  [
    count_classes_by_graph/2, % +Graph:atom
                              % -Count:nonneg
    count_instances_by_class/2 % +Class:iri
                               % -NumberOfIndividuals:nonneg
  ]
).

/** <module> RDF(S) Entailment: Statistics

Predicates that calculate statistics over RDF(S) data
for which some form of entailment is needed.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(aggregate)).

:- use_module(plRdfEntailment(api/rdfs_ent_read)).

:- rdf_meta(count_classes_by_graph(+,-)).
:- rdf_meta(count_instances_by_class(r,-)).





%! count_classes_by_graph(+Graph:atom, -Count:nonneg) is det.
% Returns the number of distinct URIs that occur in the given graph
% and that denote an RDFS class.

count_classes_by_graph(Graph, Count):-
  aggregate_all(
    set(Class),
    rdfs_class(Class, Graph),
    Classes
  ),
  length(Classes, Count).



%! count_instances_by_class(
%!   +Class:or([bnode,iri]),
%!   -NumberOfIndividuals:nonneg
%! ) is det.

count_instances_by_class(Class, N):-
  aggregate_all(
    set(Instance),
    rdfs_instance(Instance, Class),
    Instances
  ),
  length(Instances, N).

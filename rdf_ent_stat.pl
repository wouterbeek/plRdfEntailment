:- module(
  rdf_end_stat,
  [
    classes_by_graph/2, % +Graph:atom
                        % -Count:nonneg
    instances_by_class/3, % +Class:iri
                          % +Graph:atom
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

:- rdf_meta(classes_by_graph(+,-)).
:- rdf_meta(instances_by_class(r,-)).





%! classes_by_graph(+Graph:atom, -Count:nonneg) is det.
% Returns the number of distinct URIs that occur in the given graph
% and that denote an RDFS class.

classes_by_graph(Graph, Count):-
  aggregate_all(
    set(Class),
    rdfs_class(Class, Graph),
    Classes
  ),
  length(Classes, Count).



%! instances_by_class(
%!   +Class:or([bnode,iri]),
%!   -NumberOfIndividuals:nonneg
%! ) is det.

instances_by_class(Class, N):-
  aggregate_all(
    set(Instance),
    rdfs_individual(Instance, Class)
    Instances
  ),
  length(Instances, N).




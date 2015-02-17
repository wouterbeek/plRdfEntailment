% The load file for the plRdfEntailment library.

:- use_module(library(ansi_term)).

:- dynamic(user:project/2).
:- multifile(user:project/2).
   user:project(
     plRdfEntailment,
     'Library implementing RDF(S) entailment.'
   ).

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plHttp,
     plLangTag,
     plRdf,
     plSet,
     plTms,
     plTree,
     plUri,
     plXml,
     plXsd
]).

:- use_module(lodego(lodego)).

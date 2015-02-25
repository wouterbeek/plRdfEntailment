:- module(
  rdf_ent_hybrid,
  [
    rdf_lean_graph/2, % +Graph:atom
                      % -NonLeanTriple:compound
    rdf_subproperty_closure/1 % -SubPropertiesOfRdfsSubPropertyOf:list(iri)
  ]
).

/** <module> RDF Entailment: Hybrid

Hybrid implementation of RDF(S) entailment.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(closure)).
:- use_module(generics(lambda_meta)).

:- use_module(plRdf(rdf_triples)).
:- use_module(plRdf(term/rdf_instance)).





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




%! rdf_subproperty_closure(-SubPropertiesOfRdfsSubPropertyOf:list(iri)) is det.
% The properties for which the RDFS reasoning rules for
% the subproperty hierarchy apply.
%
% Algorithm
% =========
%
% Initialization
% --------------
%
% ```latex
% \begin{itemize}
%   \item $\leq_0 = \set{\leq}$
%   \item $\forall p \in P_G ( Cl_0(x) = \set{\pair{x}{x}} )$
% \end{itemize}
% ```
%
% Recursion
% ---------
%
% ```latex
% \begin{itemize}
%   \item $
%           Cl_{i+1}(x)_{\leq_i}
%         :=
%           \setdef{
%             \pair{x}{y}
%           }{
%             \exists \range{z_1}{z_n} (
%               \bigwedge_{1 \leq j < n} \exists \leq' \in \leq_i (
%                 \pair{z_j,z_{j+1}} \in \leq'
%               )
%              )
%           }
%         $
%   \item $
%           \leq_{i+1}
%         :=
%           \setdef{
%             \leq'
%           }{
%             \pair{\leq'}{\leq} \in Cl_{i+1}(\set{\leq})_{\leq_i}
%           }
%         $
% \end{itemize}
% ```

rdf_subproperty_closure(Ps):-
  rdf_global_id(rdfs:subPropertyOf, P0),
  rdf_subproperty_closure([P0], Ps).

%! rdf_subproperty_closure(
%!   +Properties:list(iri),
%!   -ClosedProperties:list(iri)
%! ) is det.
% Closes the given properties under the subproperty hierarchy.

rdf_subproperty_closure(Ps1, Ps2):-
  findall(
    P2,
    rdf_reachable_closure(rdfs:subPropertyOf, Ps1, P2),
    Ps2
  ).

%! rdf_reachable_closure(
%!   +From:rdf_term,
%!   +Properties:list(iri),
%!   -To:rdf_term
%! ) is nondet.
% Reachability closure over rdf_has_closure/3.

rdf_reachable_closure(X, Ps, Y):-
  closure(\X^rdf_has_closure(X, Ps), X, Y).

%! rdf_has_closure(
%!   +From:rdf_term,
%!   +Properties:list(iri),
%!   -To:rdf_term
%! ) is nondet.
% Direct links between From and To, closed under
% the subproperty hierarchy of the given Properties.

rdf_has_closure(X, Ps, Y):-
  member(P, Ps),
  % Either [1] belongs to the triple store or [2] and [3] do.
  %
  % ```prolog
  % [1]   rdf(X, P, Y)
  % [2]   rdf(X, P', Y)
  % [3]   rdfs_subPropertyOf(P', P)
  % ```
  rdf_has(X, P, Y).


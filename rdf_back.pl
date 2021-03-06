:- module(
  rdf_back,
  [
    rdf_back/1, % ?Triple
    rdf_back/2  % ?Triple, +Opts
  ]
).

/** <module> RDF back

Backward chaining RDF(S) reasoning.

A proof has a proof tree.
Within a proof tree, a derivation may occur multiple times.
Within a branch of a proof tree, a derivation may occur at most one time.
The latter condition holds under structural identity, i.e. =@=/2.

@author Wouter Beek
@version 2014/07, 2015/02, 2016/05
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).

%! rdf:axiom(?Regime, ?Axiom) is nondet.
%! rdf:explanation(?Regime, ?Rule, ?Explanation) is nondet.
%! rdf:regime(?Regime) is nondet.
%! result(?Triple) is nondet.

:- discontiguous
    rdf:axiom/2,
    rdf:explanation/3,
    rdf:regime/1.

:- multifile
    rdf:axiom/2,
    rdf:explanation/3,
    rdf:regime/1.

:- rdf_meta
   rdf:axiom(?, t),
   rdf_back(t),
   rdf_back(t, +).

:- thread_local
   result/1.





%! rdf_back(+Triple) is nondet.
%! rdf_back(+Triple, +Opts) is nondet.
% The following options are supported:
%   * `entailment_regimes(+list(atom))`
%     The entailment regimes whose rules are used to backward chaining.
%     Default is `[rdf]`.
%   * `graph(+atom)`
%     Retrict the facts that are considered
%     to those that appear in a specific graph.
%     Default is `user`.
%   * `multiple_justifications(+boolean)`
%     Whether the same result is returned multiple times,
%     in case multiple justifications exist.
%     Default is `false`.

rdf_back(Triple):-
  rdf_back(Triple, []).


rdf_back(Triple, Opts):-
  % Reset the previous result store, if any.
  retractall(reset(_)),
  
  % Set default options.
  option(entailment_regimes(Regimes), Opts, [rdf]),
  q_default_graph(DefG),
  option(graph(G), Opts, DefG),
  option(multiple_justifications(MultiJ), Opts, false),
  
  % Obtain a single query results.
  rule_back(Regimes, Triple, G, [], Tree),
  
  % If we want to exclude duplicate results,
  % we must record previous results.
  (MultiJ == false -> (result(Triple) -> fail ; assert(result(Triple))) ; true),
  
  % Print the query result.
  with_output_to(user_output, print_tree(Tree, [node_printer(rdf_proof_node)])).


%! rule_back(+Regimes, +Conclusion, ?G, +CurrentPath, -ProofTree) is nondet.

% [fact] All facts can be deduced.

rule_back(_, rdf(S,P,O), G, Path, (fact-rdf(S,P,O))-[]):-
  without_structural_variant(Path, axiom-rdf(S,P,O)),
  rdf(S, P, O, G).


% [se1] Existential quantification w.r.t. the object term.

rule_back(
  Regimes,
  rdf(S,P,BNode),
  Graph,
  Path1,
  (se1-rdf(S,P,BNode))-[SubTree]
):-
  memberchk(se, Regimes),
  var_or_bnode(BNode),

  without_structural_variant(Path1, se1-rdf(S,P,BNode), Path2),
  rule_back(Regimes, rdf(S,P,O), G, Path2, SubTree),

  % Use an existing mapping, if it exists.
  % Add a new mapping, otherwise.
  term_set_bnode(G, O, BNode).


% [se2] Existential quantification w.r.t. the subject term.

rule_back(
  Regimes,
  rdf(BNode,P,O),
  Graph,
  Path1,
  (se2-rdf(BNode,P,O))-[SubTree]
):-
  memberchk(se, Regimes),
  var_or_bnode(BNode),

  without_structural_variant(Path1, se2-rdf(BNode,P,O), Path2),
  rule_back(Regimes, rdf(S,P,O), Graph, Path2, SubTree),

  % Use an existing mapping, if it exists.
  % Add a new mapping, otherwise.
  term_set_bnode(Graph, S, BNode).


% [lg] Literal generalization is a special case of [se1],
%      where the object term is a literal.
%      Literal generalization is used whenever something has to be
%      predicated of a literal (since literals cannot occur
%      as subject terms).

rule_back(
  Regimes,
  rdf(S,P,BNode),
  Graph,
  Path1,
  (lg-rdf(S,P,BNode))-[SubTree]
):-
  % Rule `lg` is suprfluous under the `se` regime,
  % since rule `se1` is a generalization.
  memberchk(rdf, Regimes),
  \+ memberchk(se, Regimes),
  var_or_bnode(BNode),

  without_structural_variant(Path1, lg-rdf(S,P,BNode), Path2),
  rule_back(Regimes, rdf(S,P,literal(Literal)), Graph, Path2, SubTree),

  term_set_bnode(Graph, literal(Literal), BNode).


% [rdf1] Predicate terms are instances of =|rdf:'Property'`.

rule_back(
  Regimes,
  rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  ),
  Graph,
  Path1,
  (rdf1-rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  ))-[SubTree]
):-
  memberchk(rdf, Regimes),

  without_structural_variant(
    Path1,
    rdf1-rdf(
      P,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
    ),
    Path2
  ),
  rule_back(Regimes, rdf(_,P,_), Graph, Path2, SubTree).


% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'`.

rule_back(
  Regimes,
  rdf(
    BNode,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'
  ),
  Graph,
  Path1,
  (rdf2-rdf(
    BNode,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'
  ))-[SubTree]
):-
  memberchk(rdf, Regimes),
  var_or_bnode(BNode),

  without_structural_variant(
    Path1,
    rdf2-rdf(
      BNode,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'
    ),
    Path2
  ),
  rule_back(
    Regimes,
    rdf(
      _,
      _,
      literal(
        type(
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
          XmlLiteral
        )
      )
    ),
    Graph,
    Path2,
    SubTree
  ),

  % @tbd Check whether the values is a well-typed XML expression.

  term_set_bnode(Graph, XmlLiteral, BNode).


% [rdfs2] Class membership through domain restriction.

rule_back(
  Regimes,
  rdf(
    Instance,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    Class
  ),
  Graph,
  Path1,
  (rdfs2-rdf(
    Instance,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    Class
  ))-[SubTree1,SubTree2]
):-
  memberchk(rdfs, Regimes),
  
  without_structural_variant(
    Path1,
    rdfs2-rdf(
      Instance,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      Class
    ),
    Path2
  ),
  
  rule_back(
    Regimes,
    rdf(P,'http://www.w3.org/2000/01/rdf-schema#domain',Class),
    Graph,
    Path2,
    SubTree1
  ),
  rule_back(
    Regimes,
    rdf(_,P,_),
    Graph,
    Path2,
    SubTree2
  ).


% [axiom] All axioms can be deduced.

rule_back(Regimes, Axiom, _, Path, (axiom-Axiom)-[]):-
  % Allow axioms from any entailment regime.
  % NONDET.
  member(Regime, Regimes),

  without_structural_variant(Path, axiom-Axiom),
  rdf:axiom(Regime, Axiom).



% Helpers

rdf_proof_node(Rule-Triple) -->
  bracketed(square, atom(Rule)),
  " ",
  rdf_triple_name(Triple).


var_or_bnode(Var):-
  var(Var), !.
var_or_bnode(BNode):-
  rdf_is_bnode(BNode).


%! without_structural_variant(
%!   +Path:list(pair(atom,compound)),
%!   +Member:pair(atom,compound)
%! ) is semidet.

without_structural_variant(L, X):-
  without_structural_variant(L, X, _).

%! without_structural_variant(
%!   +Path1:list(pair(atom,compound)),
%!   +Member:pair(atom,compound),
%!   -Path2:list(pair(atom,compound))
%! ) is semidet.

without_structural_variant(T, H1, [H2|T]):-
  copy_term(H1, H2),
  (   member(X, T),
      H2 =@= X
  ->  fail
  ;   true
  ).


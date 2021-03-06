:- module(
  rdf_mat,
  [
    rdf_materialize/2 % +G, +Opts
  ]
).

/** <module> RDF materialization engine

Takes axioms, rules, and the RDF index and performs materializations.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12-2014/01, 2014/06-2014/07, 2015/02, 2016/05
*/

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).

%! rdf:axiom(?Regime:atom, ?Axiom:compound) is nondet.

:- discontiguous(rdf:axiom/2).
:- multifile(rdf:axiom/2).
:- rdf_meta(rdf:axiom(?,t)).

%! rdf:explanation(?Regime:atom, ?Rule:atom, ?Explanation:atom) is nondet.

:- discontiguous(rdf:explanation/3).
:- multifile(rdf:explanation/3).

%! rdf:rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Conclusion:compound,
%!   ?G
%! ) is nondet.

:- discontiguous(rdf:rule/5).
:- multifile(rdf:rule/5).
:- rdf_meta(rdf:rule(?,?,t,t,?)).
% All axioms can be deduced as if they are the outcome of a rule.
rdf:rule(Regime, axiom, [], Axiom, _) :-
  rdf:axiom(Regime, Axiom).

%! rdf:regime(?Regime:atom) is nondet.

:- discontiguous(rdf:regime/1).
:- multifile(rdf:regime/1).

:- predicate_options(rdf_materialize/2, 2, [
  entailment_regimes(+list(atom)),
  max_enumerator(+nonneg),
  pass_to(rdf_materialize/3, 3)
]).
:- predicate_options(rdf_materialize/3, 3, [
  entailment_regimes(+list(atom)),
  multiple_justifications(+boolean)
]).





%! rdf_materialize(?G, +Opts) is det.
% Performs all depth-one deductions for either the given RDF graph
% or no RDF graph.
%
% If a graph is given, then only triples from that graph are considered.
% Results are added to the same graph.
%
% ### Options
%
% The following options are supported:
%   - `entailment_regimes(+list(atom))`
%     Default: `[rdf,rdfs]`
%   - `max_enumerator(+nonneg)`
%     The maximum enumerator that is considered.
%     Default: `inf`.
%   - `multiple_justifications(+boolean)`
%     Whether a single proposition can have more than one justification.
%     Default: `false`.
%
% ### Regime specification
%
% Regimes are specified as an ordered set of atomic names
% denoting the entailment regimes that are used for materialization.
%
% We cannot always assume that a 'lower' regime is applicable,
% when a 'higher' regime is requested.
% For instance with `[rdf,rdfs]` we do **not** intend to use `se`.

% No materialization whatsoever.
rdf_materialize(_, Opts) :-
  option(entailment_regimes(Regimes), Opts),
  memberchk(none, Regimes), !.
% Some form of materialization.
rdf_materialize(G, Opts1) :-
  % Reset flag used for debugging.
  flag(deductions, _, 0),

  % The default graph is called `user`.
  % This is also the default graph that rdf/3 writes to.
  default(user, G),

  % Make sure there is an existing/registered TMS that can be used.
  % Otherwise, we have to create a new TMS for this purpose.
  % We choose a TMS according to Doyle's orginal specification (classic!).
  graph_tms(G, Tms),

  select_option(max_enumerator(High), Opts1, Opts2, inf),
  temporarily_set_setting(
    rdf:max_enumerator,
    High,
    rdf_materialize(Tms, G, Opts2)
  ).

%! rdf_materialize(+TMS:atom, +G, +Opts) is nondet.
% The inner loop of materialization.
% This performs all depth-1 reasoning steps (i.e., breadth-first).

rdf_materialize(Tms, G, Opts) :-
  option(entailment_regimes(Regimes), Opts, [rdf,rdfs]),

  % NONDET
  member(Regime, Regimes),
  % NONDET
  rdf:rule(Regime, Rule, Premises, rdf(S,P,O), G),

  % Only accept new justifications.
  % A proposition may have multiple justifications.
  (   option(multiple_justifications(true), Opts, false)
  ->  % @tbd Checking for existing justifications does not work yet.
      \+ tms_justification(Tms, Premises, Rule, rdf(S,P,O))
  ;   \+ rdf(S, P, O, G)
  ),

  % Add to TMS.
  doyle_add_argument(Tms, Premises, Rule, rdf(S,P,O), Justification),

  % DEB
  % @tbd debug/1 does not work.
  % Put this under if_debug/2.
  dcg_with_output_to(atom(Msg), materialize_message(Tms, Justification)),
  format(user_output, '~a\n', [Msg]),

  % Store the result.
  qb(M, S, P, O, G),
  fail.
% Done!
rdf_materialize(_, _, Opts) :-
  % DEB
  % @tbd Use if_debug/2.
  option(entailment_regimes(Regimes), Opts, [rdf,rdfs]),
  dcg_with_output_to(atom(RegimesAtom), list(pl_term, Regimes)),
  flag(deductions, N, 0),
  format(user_output, 'Added ~w deductions (regimes: ~w).', [N,RegimesAtom]).



% Helpers

%! graph_tms(+G, -Tms:atom) is det.
% Returns either the existing TMS for the given graph,
% or creates and returns a new one.

graph_tms(G, Tms) :-
  atomic_list_concat([tms,G], '_', Tms),
  ensure_tms(Tms).

ensure_tms(Tms) :-
  tms(Tms), !.
ensure_tms(Tms) :-
  register_tms(doyle, Tms),
  doyle_reset(Tms),
  doyle_init(Tms).


%! materialize_message(+Tms:atom, +Justification:compound)// is det.

materialize_message(Tms, Justification) -->
  {flag(deductions, Id, Id + 1)},
  integer(Id),
  ": ",
  tms_print_justification(
    Tms,
    Justification,
    [indent(0),language_preferences([en])]
  ).


% All axioms can be deduced.
rdf:rule(Regime, axiom, [], Axiom, _) :-
  rdf:axiom(Regime, Axiom).
% All facts can be deduced.
rdf:rule(_, fact, [], rdf(S,P,O), G) :-
  rdf(S, P, O, G).

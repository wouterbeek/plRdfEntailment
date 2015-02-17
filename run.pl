% Standalone startup file for the plRdfEntailment library.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

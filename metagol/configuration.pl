:-module(configuration, [dynamic_predicates/1
                        ,learner/1
                        ,learning_predicate/1
                        ,metagol_data_file/1
                        ,metagol_data_directory/1
                        ,named_metarule/2
                        ]).

:-reexport(lib(evaluation/evaluation_configuration)).
:-reexport(lib/sampling/sampling_configuration).

/** <module> Configuration options for Metagol shim layer.
*/

% Declared multifile to allow learn/5 to be added to Metagol's learning
% predicates' interface.
:-multifile learn/5
           ,learn/2
           ,learn/3.

% Declared dynami to allow programmatic manipulation during experiment
% execution.
:-dynamic metagol_data_directory/1
         ,metagol_data_file/1.

% Declared multifile to allow named_metarule/2 definitions to be loaded
% from Louise experiment files.
:-multifile named_metarule/2.

%!      dynamic_predicates(?Predicates) is semidet.
%
%       Predicates manipulated dynamically by Metagol.
%
%       This list is used to ensure thorough cleanup of the dynamic
%       database between learning attempts with Metagol.
%
%       @tbd Note that Metagol options max_clauses/1, min_clauses/1 and
%       max_inv_preds/1 that are declared multifile should not be
%       modified dynamically and so are not included in this list.
%
%       @tbd Not actually used anywhere, except in cleanup_dynamics/0,
%       via cleanup_experiment/0, which is currently just a unit clause
%       and so does nothing at all. See cleanup_experiment/0 for a
%       quasi-attempt at a half-explanation.
%
dynamic_predicates([metagol:ibk/3
                   ,metagol:functional/0
                   ,user:head_pred/1
                   ,user:body_pred/1
                   ,metagol:body_pred_call/2
                   ,metagol:compiled_pred_call/2
                    % NOT DYNAMIC!
                   %,metagol:max_clauses/1
                   %,metagol:min_clauses/1
                   %,metagol:max_inv_preds/1
                   %,user:func_test/3
                   ,metagol:metarule/6
                   ,metagol:metarule/3
                   ,metagol:type/3
                   ,metagol:metarule_next_id/1
                   ]).


%!	learner(?Name) is semidet.
%
%	Name of the learning system this configuration is for.
%
%	Name is one of [louise,thelma,metagol].
%
%       Used to switch context between Metagol, Louise and Thelma, where
%       this is needed. The typical use case is when experiment code
%       must check the values of configuration options that are
%       particular to one or the other system (e.g. resolutions/1 is not
%       present in Thelma etc).
%
learner(metagol).


%!	learning_predicate(+Learning_Predicate) is semidet.
%
%	The Learning_Predicate to be used in list_learning_results/0.
%
%	Learning_Predicate is a predicate indicator, the symbol and
%	arity of one of the following learning predicates defined in
%	Louise: learn/1, learn_dynamic/1 or
%	learn_with_examples_invention/2. The specified predicate will be
%	used to list the learning results for all learning targets
%	defined in an experiment file with a call to
%	list_learning_results/0.
%
%	learning_predicate/1 is declared as multifile. To specify the
%	learning predicate to be used with list_learning_results/0, add
%	a clause of learning_predicate/1 to the relevant experiment
%	file.
%
%	For example, the following clause:
%	==
%	configuration:learning_predicate(learn_dynamic/1).
%	==
%
%	Will cause list_learning_results/0 to use learn_dynamic/1 for
%	all predicates in the experiment file containing that clause.
%
%	learning_predicate/1 is declared dynamic. You do not have to
%	specify a learning predicate for every experiment file.
%	list_learning_results/0 will default to learn/1.
%
%	Note that learning_predicate/1 will not affect learning by
%	calling learning predicates directly. That is, having added a
%	clause of learning_predicate/1 like the one above to an
%	experiment file you are free to then call learn/1 or any other
%	learning predicate on any of the learning targets in that
%	experiment file. Only the learning predicate used by
%	list_learning_results/0 is affected by this option.
%
%	Finally, note that specifying any other predicate than the three
%	learning predicates listed above as a learning_predicate will
%	cause list_learning_results/0 to raise an error.
%
%	@see list_learning_results/0
%
:-dynamic learning_predicate/1.
:-multifile learning_predicate/1.
learning_predicate(hypotheses_union/5).


%!      max_clauses(?Max) is semidet.
%
%       Metagol option: maximum hypothesis cardinality.
%
metagol:max_clauses(4).


%!      max_inv_preds(?Max) is semidet.
%
%       Metagol option: maximum number of invented predicates' clauses.
%
metagol:max_inv_preds(0).


%!      min_clauses(?Min) is semidet.
%
%       Metagol option: minimum hypothesis cardinality.
%
metagol:min_clauses(1).


%!      metagol_data_directory(?Path) is semidet.
%
%       Path to the root directory for Metagol data files.
%
metagol_data_directory(data(experiment)).


%!      metagol_data_file(?Path) is semidet.
%
%       Path to the current Metagol data file.
%
%metagol_data_file(data(examples/tiny_kinship)).
metagol_data_file(data(experiment/kin)).
%metagol_data_file(data(experiment/mtg_fragment)).
%metagol_data_file(data(experiment/robots)).
%metagol_data_file(data(experiment/path_no_noise)).


%!      named_metarule(?Name, ?Metarule) is semidet.
%
%       A named Metarule in Metagol's internal format.
%
%       @tbd Metarules translated from Louise's configuration.
%
named_metarule(abduce, metarule([P,X,Y], [P,X,Y], [])).
named_metarule(unit, metarule([P], [P,_X,_Y], [])).
named_metarule(projection_21, metarule([P,Q], [P,X,X], [[Q,X]])).
named_metarule(projection_21, metarule([P,Q], [P,X], [[Q,X,X]])).
named_metarule(identity, metarule([P,Q], [P,X,Y], [[Q,X,Y]])).
named_metarule(inverse, metarule([P,Q], [P,X,Y], [[Q,Y,X]])).
named_metarule(chain, metarule([P,Q,R], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(tailrec, metarule([P,Q], [P,X,Y], [[Q,X,Z],[P,Z,Y]])).
named_metarule(precon, metarule([P,Q,R], [P,X,Y], [[Q,X],[R,X,Y]])).
named_metarule(postcon, metarule([P,Q,R], [P,X,Y], [[Q,X,Y],[R,Y]])).
named_metarule(switch, metarule([P,Q,R], [P,X,Y], [[Q,X,Z],[R,Y,Z]])).
named_metarule(swap, metarule([P,Q,R], [P,X,Y], [[Q,Z,X],[R,Z,Y]])).
named_metarule(leftrec, metarule([P,Q], [P,X,Y], [[P,X,Z],[Q,Z,Y]])).
named_metarule(allrec, metarule([P], [P,X,Y], [[P,X,Z],[P,Z,Y]])).
named_metarule(chain_abduce_x, metarule([P,Q,R,X], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(chain_abduce_y, metarule([P,Q,R,Y], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(chain_abduce_z, metarule([P,Q,R,Z], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(projection_21_abduce, metarule([P,Q,X], [P,X,X], [[Q,X]])).
named_metarule(projection_21_abduce, metarule([P,Q,X], [P,X], [[Q,X,X]])).

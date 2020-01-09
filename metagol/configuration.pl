:-module(configuration, [experiment_file/2
                        ,dynamic_predicates/1
                        ,learner/1
                        ,max_clauses/1
                        ,max_inv_preds/1
                        ,min_clauses/1
                        ,metagol_data/1
                        ,named_metarule/2
                        ]).

:-user:use_module(src(experiment_file)).
:-reexport(lib(evaluation/evaluation_configuration)).
:-reexport(lib/sampling/sampling_configuration).

/** <module> Configuration options for Louise compatibility layer.

*/

%:-debug(metagol).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
%experiment_file('data/examples/anbn.pl',anbn).


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


%!      max_clauses(?Maximum) is semidet.
%
%       Maximum hypothesis cardniality.
%
%       @tbd Metagol option declared multifile in module metagol. The
%       normal way to set this option is to add it directly into a data
%       file and load the file. Editing the configuration module is the
%       preferred way in the Louise compatibility layer, instead.
%
metagol:max_clauses(40).

% Repeated for the sake of print_config/3
max_clauses(Max):-
        metagol:max_clauses(Max).


%!      max_inv_preds(?Maximum) is semidet.
%
%       Maximum number of invented predicate symbols.
%
%       @tbd Metagol option declared multifile in module metagol. The
%       normal way to set this option is to add it directly into a data
%       file and load the file. Editing the configuration module is the
%       preferred way in the Louise compatibility layer, instead.
%
metagol:max_inv_preds(0).

% Repeated for the sake of print_config/3
max_inv_preds(Max):-
        metagol:max_inv_preds(Max).


%!      min_clauses(?Minimum) is semidet.
%
%       Minimum hypothesis cardinality.
%
%       @tbd Metagol option declared multifile in module metagol. The
%       normal way to set this option is to add it directly into a data
%       file and load the file. Editing the configuration module is the
%       preferred way in the Louise compatibility layer, instead.
%
metagol:min_clauses(1).

% Repeated for the sake of print_config/3
min_clauses(Max):-
        metagol:min_clauses(Max).



%!      metagol_data(?Path) is semidet.
%
%       Path to the root directory for Metagol data files.
%
metagol_data(data(experiment)).


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
named_metarule(leftrec, metarule([P,Q], [P,X,Y], [[P,X,Z],[Q,Z,Y]])).
named_metarule(allrec, metarule([P], [P,X,Y], [[P,X,Z],[P,Z,Y]])).
named_metarule(chain_abduce_x, metarule([P,Q,R,X], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(chain_abduce_y, metarule([P,Q,R,Y], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(chain_abduce_z, metarule([P,Q,R,Z], [P,X,Y], [[Q,X,Z],[R,Z,Y]])).
named_metarule(projection_21_abduce, metarule([P,Q,X], [P,X,X], [[Q,X]])).
named_metarule(projection_21_abduce, metarule([P,Q,X], [P,X], [[Q,X,X]])).


% Loads the current experiment file in the Swi-Prolog IDE when the
% configuration is changed.
%
% It is perfectly safe to remove this directive.
%
%:-experiment_file(P,_)
%  ,edit(P).


% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:-experiment_file:reload.

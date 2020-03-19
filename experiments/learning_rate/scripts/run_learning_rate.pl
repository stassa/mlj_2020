:-module(run_learning_rate, [write_dataset/1
                            ,run_kin/0
                            ,run_mtg_fragment/0
                            ,run_robots/0
                            ]).

/** <module> Running script for learning_rate.pl experiment.

Predicates in this module formalise running of learning curve
experiments comparing Louise to Metagol detailed in the paper.

Directives called when this module file is loaded set configuration
options and other necessary parameters and should also act as
documentation of those options for future reference and to repeat the
experiments.

Note that the experiment run by learning_rate.pl is named as a "learning
curve" experiment in the paper. This module was misnamed originally and
now it's too much work to change its name and every reference to its
name everywhere.
*/

% move/2 programs learned by Louise with a high sampling rate can be
% quite large (over 2k clauses for a 4x4 grid) and that blows the
% tabling space during evaluation (we table the target predicate of a
% problem to avoid going infinite with left-recursion during
% evaluation). 8GB seems to be enough of an increase from the default
% Swi-Prolog table space to allow evaluation to complete without errors.
:-set_prolog_flag(table_space, 8_589_934_592).

% This script must be run with the current working directory set to the
% top directory of Metagol or Louise. The working directory
% determines which learner will run the experiment. Running this script
% in <project_root>/metagol/ will run the experiments with Metagol.
% Running it in <project_root>/louise/ will run the experiment with
% Aleph.
%
% Just checking you're paying attention. Louise, not Aleph.
:-[load_headless].

% Search paths relative to the ones set in load_headless.pl in
% <project_root>/metagol/ or <project_root>/louise/
user:file_search_path(experiment_data, project_root('../data')).
user:file_search_path(experiments, project_root('../experiments')).
user:file_search_path(learning_rate, experiments(learning_rate)).

% Experiment code and necessary libraries.
:-use_module(learning_rate).
:-use_module(learning_rate_configuration).
:-use_module(lib(mathemancy/mathemancy)).

% Allow dynamic setting of normally static options in Louise.
:-dynamic configuration:experiment_file/2
         ,configuration:minimal_program_size/2.

% Colorise Swi debug messages to make them more readable in
% dark-coloured terminals (default colouring is too dark).
user:message_property(debug(learning_rate), color( [ fg(cyan) ]) ).
user:message_property(debug(learning_rate_setup), color( [ fg(cyan) ]) ).
user:message_property(debug(run_learning_rate_setup), color( [ fg(cyan) ]) ).
user:message_property(debug(progress), color( [ fg(yellow) ]) ).

% Enables logging of experiment progress to console.
% Turning this off improves running times but it makes it harder to know
% when an experiment has finished running.
:-debug(run_learning_rate_setup).

% Avoids some of the warnings when consulting metagol.pl
:-dynamic metagol:max_clauses/1
         ,metagol:max_inv_preds/1
         ,metagol:min_clauses/1
         ,metagol:metarule/6.


%!      config(?Dataset,?Option,?Value) is semidet.
%
%       Dataset configuration options.
%
%       These are configuration options normally set manually in
%       configuration.pl in the root directories of Metagol and Louise
%       or learning_rate_configuration.pl (the configuration file for
%       the script that actually runs the experiment). These options are
%       documented in their respective configuration files.
%
%       The point of declaring these options here is to keep all the
%       configuration options needed to run the experiments in one
%       place, so that it's easier to reproduce the experiment without
%       having to fiddle with different configuration options.
%
%       If a user wishes to change the configuration (to test different
%       experimental setups) it is recommended to do it by changing
%       these configuration options, rather than the configuration files
%       where these are defined (which would take some looking up
%       anyway).
%
/* kin.pl config options */
config(kin,experiment_file,['../data/kinship/kin.pl',kin]).
config(kin,copy_plotting_scripts,[learning_rate(plotting)]).
config(kin,logging_directory,'../experiments/learning_rate/output/kin/').
config(kin,plotting_directory,'../experiments/learning_rate/output/kin/').
config(kin,learning_predicate,[learn/5]).
config(kin,learning_rate_time_limit,[300]).
config(kin,minimal_program_size,[2,inf]).
config(kin,reduction,[plotkins]).
config(kin,resolutions,[5000]).
config(kin,recursive_reduction,[true]).
% Metagol options
config(kin,max_clauses,[40]).
config(kin,max_inv_preds,[0]).
config(kin,min_clauses,[1]).
% mtg_fragment.pl options
config(mtg_fragment,experiment_file,['../data/mtg/mtg_fragment.pl',mtg_fragment]).
config(mtg_fragment,copy_plotting_scripts,[learning_rate(plotting)]).
config(mtg_fragment,logging_directory,'../experiments/learning_rate/output/mtg_fragment/').
config(mtg_fragment,plotting_directory,'../experiments/learning_rate/output/mtg_fragment/').
config(mtg_fragment,learning_predicate,[learn/5]).
config(mtg_fragment,learning_rate_time_limit,[300]).
config(mtg_fragment,minimal_program_size,[2,inf]).
config(mtg_fragment,depth_limits,[40,0]).
config(mtg_fragment,reduction,[none]).
config(mtg_fragment,resolutions,[5000]).
config(mtg_fragment,recursive_reduction,[true]).
% Metagol options
config(mtg_fragment,max_clauses,[40]).
config(mtg_fragment,max_inv_preds,[0]).
config(mtg_fragment,min_clauses,[1]).
% robots.pl options
config(robots,experiment_file,['../data/robots/robots.pl',robots]).
config(robots,copy_plotting_scripts,[learning_rate(plotting)]).
config(robots,logging_directory,'../experiments/learning_rate/output/robots/').
config(robots,plotting_directory,'../experiments/learning_rate/output/robots/').
config(robots,learning_predicate,[learn/5]).
config(robots,learning_rate_time_limit,[300]).
config(robots,minimal_program_size,[2,inf]).
config(robots,reduction,[plotkins]).
config(robots,resolutions,[5000]).
config(robots,recursive_reduction,[true]).
% Metagol options
config(robots,max_clauses,[40]).
config(robots,max_inv_preds,[0]).
config(robots,min_clauses,[1]).
% robots/move_generator.pl options
config(robots,experiment_world,[empty_world]).
config(robots,world_dimensions,[4,4]).


%!      setup(+Dataset) is det.
%
%       Set confiuration options for a Dataset.
%
%       This predicate first reads configuration options defined in
%       config/3 clauses in this file, then sets them dynamically using
%       the predicates set_configuration_option/2 and
%       set_local_configuration_option/3.
%
%       set_configuration_option/2 is used to set generic configuration
%       options that can also be set manually in the configuration.pl
%       file under the root directory of each of the two learners.
%
%       set_local_configuration_option/3 is used to set configuration
%       options for a) learning_rate.pl (the experiment code module) in
%       particular, i.e. the options in learning_rate_configuration.pl,
%       and b) experiment files' configuration modules, in particular
%       the configuration for the move generator in the grid world
%       navigation experiment generator_configuration.pl. A separate
%       predicate is necessary for the experiment code because
%       experiment code and dataset configuration options are not
%       accessible to set_configuration_option/2. The reason for this
%       inaccessibility is that set_configuration_option/2 calls
%       configuration option predicates directly in the configuration
%       module, with a prefix of "configuration:" (note the colon),
%       whereas experiment code and experiment file configuration
%       options are declared in their own configuration modules that are
%       not imported into the main configuration module (to reduce the
%       configuration module's number of options).
%
%       The reason for all this indirection is to allow experiments to
%       be run without having to touch the code of the two learners.
%       This ensures that all the configuration options that need to be
%       set to run the experiments can be managed from a single place,
%       this module.
%
setup(D):-
        % Read configuration options (defined in this file)
        configuration:learner(L)
        ,config(D,experiment_file,Es)
        ,config(D,copy_plotting_scripts,CS)
        ,config(D,logging_directory,LD)
        ,config(D,plotting_directory,PD)
        ,config(D,learning_predicate,LP)
        ,config(D,learning_rate_time_limit,TL)
        ,config(D,minimal_program_size,MP)
        ,config(D,reduction,R)
        ,config(D,resolutions,S)
        ,config(D,recursive_reduction,RR)
        % metagol options
        ,config(D,max_clauses,Max_C)
        ,config(D,max_inv_preds,MI)
        ,config(D,min_clauses,Min_C)
        % Generator options for robots dataset.
        ,(   D = robots
         ->  config(D,experiment_world,EW)
            ,config(D,world_dimensions,WD)
         ;   true
         )
        % Set configuration options dynamically to the ones defined in this file.
        ,auxiliaries:set_configuration_option(experiment_file,Es)
        ,set_local_configuration_option(learning_rate,copy_plotting_scripts,CS)
        ,set_local_configuration_option(learning_rate,logging_directory,LD)
        ,set_local_configuration_option(learning_rate,plotting_directory,PD)
        ,set_local_configuration_option(learning_rate,learning_rate_time_limit,TL)
        ,(   D = robots
         ->  set_local_configuration_option(robots,experiment_world,EW)
            ,set_local_configuration_option(robots,world_dimensions,WD)
         ;   true
         )
        ,(   L = louise
         ->  set_configuration_option(reduction,R)
            ,set_configuration_option(resolutions,S)
            ,set_configuration_option(recursive_reduction,RR)
            ,set_configuration_option(learning_predicate,LP)
            ,set_configuration_option(minimal_program_size,MP)
         ;   L = metagol
         ->  set_configuration_option(metagol_data_file,[data(experiment/D)])
            ,set_local_configuration_option(metagol,max_clauses,Max_C)
            ,set_local_configuration_option(metagol,max_inv_preds,MI)
            ,set_local_configuration_option(metagol,min_clauses,Min_C)
         ;   format(atom(E),'Unknown learner: ~w',[L])
            ,throw(E)
         ).



%!      write_dataset(+Problem) is det.
%
%       Write the Metagol dataset for the given Problem.
%
%       Problem is the name of a dataset representing a MIL problem
%       defined in an experiment file module in the format expected by
%       Louise and used in a learning rate experiment. The module name
%       of the experiment file is the same as Problem and one of: [kin,
%       robots, mtg_fragment].
%
%       Experiment files used in the learning rate experiment were
%       originally created for Louise. This predicate handles their
%       translation to the representation of a MIL problem required by
%       Metagol. It first calls setup/1 to set configuration options as
%       necessary to run the experiment.
%
%       The Metagol data file written has the same name as Problem (with
%       the extension .pl).
%
write_dataset(D):-
        % Mapping of dataset name to learning target.
        % D is also the module name of the dataset.
        memberchk(D-T,[kin-kin/2
                      ,mtg_fragment-ability/2
                      ,robots-move/2
                      ])
        % Setup necassary configuration options.
        ,setup(D)
        % For the robots problem we need to generate a grid world first.
        ,(   D == robots
         ->  use_module('../data/robots/move_generator.pl')
            ,debug(learning_rate_setup,'Generated grid world.',[])
            ,move_generator:write_dataset % loaded by robots.pl
         ;   true
         )
        % Only in metagol shim auxiliaries.pl
        ,auxiliaries:metagol_data_file(D,_Dir,_Fn,OP)
        % Get name of module to print out relative path
        ,module_property(run_learning_rate, file(M))
        ,(   exists_file(OP)
         ->  true
         ;   relative_file_name(OP,M,R)
            ,debug(learning_rate_setup,'Did not find dataset ~w',[R])
            % Experiment file path and module name should be written
            % to the dynamic database by setup/1.
            ,configuration:experiment_file(IP,D)
            ,write_metagol_dataset(IP,D,T)
            ,debug(learning_rate_setup,'Wrote dataset ~w',[R])
         ).



%!      run_kin is det.
%
%       Run a learning rate experiment on the kin.pl dataset.
%
run_kin:-
        configuration:learner(L)
        ,once(setup(kin))
        ,T = kin/2
        ,M = acc
        ,K = 100
        ,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        %,K = 5
        %,interval(1,10,1,Ss)
        ,debug(progress,'~w: Starting on kin dataset',[L])
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with kin dataset',[L]).



%!      run_mtg_fragment is det.
%
%       Run a learning rate experiment on the mtg_fragment.pl dataset.
%
run_mtg_fragment:-
        configuration:learner(L)
        ,once(setup(mtg_fragment))
        ,T = ability/2
        ,M = acc
        ,K = 100
        ,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        %,K = 5
        %,interval(1,10,1,Ss)
        ,debug(progress,'~w: Starting on mtg_fragment dataset',[L])
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with mtg_fragment dataset',[L]).



%!      run_robots is det.
%
%       Run a learning rate experiment on the robots.pl dataset.
%
%       Note that the MIL problem in robots.pl is described as "grid
%       world navigation" in the paper.
%
run_robots:-
        configuration:learner(L)
        ,once(setup(robots))
        ,T = move/2
        ,M = acc
        ,K = 10
        ,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        %,K = 5
        %,interval(1,10,1,Ss)
        ,debug(progress,'~w: Starting on robots dataset',[L])
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with robots dataset',[L]).


%!      float_interval(+I,+K,+J,-Ss) is det.
%
%       Generate a list of floats.
%
%       Ss is a list of floats generated by first generating the numbers
%       in the closed interval [I,K] increasing by stride J, then
%       dividing each by 10.
%
%       Used to generate a list of sampling sizes for experiments.
%
float_interval(I,K,J,Ss):-
        interval(I,K,J,Is)
        ,findall(S,(member(I_,Is)
                   ,S is I_ /10)
                ,Ss).


%!      set_local_configuration_option(+Module,+Name,+Value) is det.
%
%       Set the Value of the Name'd option in a configuration Module.
%
%       Counterpart to set_configuration_option/2 defined in Louise,
%       used to dynamically set configuration options for the learning
%       rate experiment script and datasets used in learning rate
%       experiments.
%
%       Use this to set configuration options defined in
%       learning_rate_configuration.pl and datasets, particularly
%       robots.pl.
%
set_local_configuration_option(M,N,V):-
	atomic(V)
	,!
	,set_local_configuration_option(M,N,[V]).
set_local_configuration_option(M,N,Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(M:T)
	,assert(M:T_).

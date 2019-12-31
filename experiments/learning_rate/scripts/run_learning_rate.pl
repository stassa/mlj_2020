:-module(run_learning_rate, [run_kin/0
                            ,run_mtg_fragment/0
                            ,run_robots/0
                            ]).

% Must be run in the top directory of thelma or louise.
% Running in thelma/ will run the experiments with thelma.
% Running in louise/ will run the experiment with Aleph.
% Nah, just kidding. Louise.
:-[load_headless].

:-use_module(learning_rate).
:-use_module(learning_rate_configuration).
% Allow dynamic setting of normally static options in Thelma.
:-dynamic configuration:experiment_file/2
         ,configuration:depth_limits/2.
:-use_module(experiment_data(robots/move_generator)).

% Make Swi debug messages readable in default-coloured powershell.
user:message_property(debug(learning_rate), color( [ fg(cyan) ]) ).
user:message_property(debug(progress), color( [ fg(yellow) ]) ).


%!      config(?Dataset,?Option,?Value) is semidet.
%
%       Dataset configuration options.
%
/* kin.pl config options */
config(kin,experiment_file,['../data/kinship/kin.pl',kin]).
config(kin,copy_plotting_scripts,[learning_rate(plotting)]).
config(kin,logging_directory,'../experiments/learning_rate/output/kin/').
config(kin,plotting_directory,'../experiments/learning_rate/output/kin/').
config(kin,learning_rate_time_limit,[300]).
config(kin,depth_limits,[40,0]).
config(kin,reduction,[plotkins]).
config(kin,resolutions,[5000]).
config(kin,recursive_reduction,[true]).
% mtg_fragment.pl options
config(mtg_fragment,experiment_file,['../data/mtg/mtg_fragment.pl',mtg_fragment]).
config(mtg_fragment,copy_plotting_scripts,[learning_rate(plotting)]).
config(mtg_fragment,logging_directory,'../experiments/learning_rate/output/mtg_fragment/').
config(mtg_fragment,plotting_directory,'../experiments/learning_rate/output/mtg_fragment/').
config(mtg_fragment,learning_rate_time_limit,[300]).
config(mtg_fragment,depth_limits,[40,0]).
config(mtg_fragment,reduction,[none]).
config(mtg_fragment,resolutions,[5000]).
config(mtg_fragment,recursive_reduction,[true]).
% robots.pl options
config(robots,experiment_file,['../data/robots/robots.pl',robots]).
config(robots,copy_plotting_scripts,[learning_rate(plotting)]).
config(robots,logging_directory,'../experiments/learning_rate/output/robots/').
config(robots,plotting_directory,'../experiments/learning_rate/output/robots/').
config(robots,learning_rate_time_limit,[300]).
config(robots,depth_limits,[40,0]).
config(robots,reduction,[plotkins]).
config(robots,resolutions,[5000]).
config(robots,recursive_reduction,[true]).
% robots/move_generator.pl options
config(robots,experiment_world,[empty_world]).
config(robots,world_dimensions,[4,4]).


%!      setup(+Dataset) is det.
%
%       Set confiuration options for a Dataset.
%
setup(D):-
        configuration:learner(L)
        ,config(D,experiment_file,Es)
        ,config(D,copy_plotting_scripts,CS)
        ,config(D,logging_directory,LD)
        ,config(D,plotting_directory,PD)
        ,config(D,learning_rate_time_limit,TL)
        ,config(D,depth_limits,DL)
        ,config(D,reduction,R)
        ,config(D,resolutions,S)
        ,config(D,recursive_reduction,RR)
        ,(   D = robots
         ->  config(D,experiment_world,EW)
            ,config(D,world_dimensions,WD)
         ;   true
         )
        ,set_configuration_option(experiment_file,Es)
        ,set_local_configuration_option(learning_rate,copy_plotting_scripts,CS)
        ,set_local_configuration_option(learning_rate,logging_directory,LD)
        ,set_local_configuration_option(learning_rate,plotting_directory,PD)
        ,set_local_configuration_option(learning_rate,learning_rate_time_limit,TL)
        ,(   L = thelma
         ->  set_configuration_option(depth_limits,DL)
         ;   L = louise
         ->  set_configuration_option(reduction,R)
            ,set_configuration_option(resolutions,S)
            ,set_configuration_option(recursive_reduction,RR)
         ;   format(atom(E),'Unknown learner: ~w',[L])
            ,throw(E)
         )
        ,(   D = robots
         ->  set_local_configuration_option(robots,experiment_world,EW)
            ,set_local_configuration_option(robots,world_dimensions,WD)
         ;   true
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
        %,float_interval(1,9,1,Ss)
        ,interval(1,10,1,Ss)
        ,debug(progress,'~w: Starting on mtg_fragment dataset',[L])
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with mtg_fragment dataset',[L]).



%!      run_robots is det.
%
%       Run a learning rate experiment on the robots.pl dataset.
%
run_robots:-
        configuration:learner(L)
        ,once(setup(robots))
        ,T = move/2
        ,M = acc
        ,K = 10
        ,float_interval(1,9,1,Ss)
        %,interval(1,10,1,Ss)
        ,move_generator:write_dataset
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
float_interval(I,K,J,Ss):-
        interval(I,K,J,Is)
        ,findall(S,(member(I_,Is)
                   ,S is I_ /10)
                ,Ss).


%!      set_configuration_option(+Name, +Value) is det.
%
%       Set the Value of the Name'd configuration option.
%
%       Counterpart to set_configuration_option/2, to dynamically set
%       learning rate experiment configuration options.
%
%       Use this to set configuration options defined in
%       learning_rate_configuration.pl.
%
set_local_configuration_option(N, V):-
	atomic(V)
	,!
	,set_local_configuration_option(N,[V]).
set_local_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(learning_rate_configuration:T)
	,assert(learning_rate_configuration:T_).


%!      set_local_configuration_option(+Module,+Name,+Value) is det.
%
%       Set the Value of the Name'd option in a configuration Module.
%
%       Counterpart to set_configuration_option/2, to dynamically set
%       configuration options for the learning rate experiment script
%       and datasets used in learning rate experiments.
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

:-module(learning_rate, [learning_rate/6
                        ]).

:-use_module(configuration).
:-use_module(learning_rate_configuration).
:-use_module(lib(evaluation/evaluation)).
:-use_module(lib(mathemancy/mathemancy)).
:-use_module(src(auxiliaries)).

/** <module> Experiment script for learnign rate experiments.

A "learning rate" experiment varies the number of training examples as
the experiment progresses. The results can then be plotted in a graph
mapping the number (or proportion) of training examples on the x-axis
and the error (or other evaluation metric) on the y-axis.

The predicate learning_rate/6 exported by this module takes in as
arguments: the symbol and arity of a target predicate, an evaluation
metric (as defined in module louise/lib/evaluation/evaluation.pl), a
number indicating the steps for the experiment to run and a list of
numbers indicating sampling rates for the steps of the experiment. It
outputs the means and standard deviations of the experiment's results in
the specified metric.

Note that the MIL problem for the target of a learning rate experiment
must be loaded to memory and accessible to module user before the
experiment begins to avoid existence errors. The ideal way to do this is
to load your desired file through the configuration's experiment_file/2
option.

Here's an example query, running a short, two-step experiment while
sampling one and two examples of the target predicate, ancestor/2 in
each step:

==
?- debug(progress).
true.

?- debug(learning_rate).
true.

?- _T = ancestor/2, _M = acc, _K = 2, interval(1,2,1,_Ss), learning_rate(_T,_M,_K,_Ss,_Ms,_SDs), writeln(_Ms), writeln(_SDs).
% Step 1 of 2
% Sampling size: 1
% Sampling size: 2
% Step 2 of 2
% Sampling size: 1
% Sampling size: 2
% louise.acc.mean <- c(0.7777777777777778,0.65625)
% louise.acc.sd <- c(0.0,0.2209708691207961)
[0.7777777777777778,0.65625]
[0.0,0.2209708691207961]
true.
==

Note the two calls to debug/1 before the experiment query, proper.
Although debugging is enabled in this module, if the configuration
module is loaded first, the directive :-nodebug(_) near its beginning
may turn debugging for this experiment off.

Here's a similar query, this time defining the sampling rate as a
proportion of all examples, rather than an integer:

==
?- _T = ancestor/2, _M = acc, _K = 2, interval(1,2,1,_Is), findall(S,(member(I,_Is), S is I /10),_Ss), learning_rate(_T,_M,_K,_Ss,_Ms,_SDs), writeln(_Ms), writeln(_SDs).
% Step 1 of 2
% Sampling size: 0.1
% Sampling size: 0.2
% Step 2 of 2
% Sampling size: 0.1
% Sampling size: 0.2
% louise.acc.mean <- c(0.7777777777777778,0.875)
% louise.acc.sd <- c(0.0,0.1767766952966369)
[0.7777777777777778,0.875]
[0.0,0.1767766952966369]
true.
==

Note that learning_rate/6 can only currently test static learning
predicates learn/1, learn/2 and learn/5. This is a limitation of the
evaluation module, to be addressed in future work.

Plotting results
----------------

In the example queries above, the experiment predicate in this module,
learning_rate/6 outputs a list of the means and standard deviations of
the evaluation results for the experiment. Additionally, a pair of R
vectors is written to the logging stream (which, in the example, is the
console). The same pair of R vectors, plus a couple of other
variables is also written to the designated r_data_file/1. The data
written in that file can then be used to plot the experiment by invoking
the R script in:

==
louise/scripts/plotting/plot_learning_rate_results.r
==

The learning_rate_results.r script is an R script and must be run with
R. Any version after 3.4.3 will do. The script takes care of loading the
data file specified in r_data_file/1 from its local context, but the R
environment must be changed to the directory where the two R files are
placed.

Additionally, a script named plot_learning_rate_results_comparison.r in
the same directory as above is provided that is meant to compare the
performance of Louise to Thelma on respective learning rate experiments.
The data for taht experiment is not written to a convenient data file
automatically and must be harvested from the data files written while
running the experiments for those two learners separately.

*/

% ================================================================================
% Experiment initialisation
% ================================================================================

%!	init_log_dir is det.
%
%	Create the logging directory if it does not exist.
%
init_log_dir:-
	learning_rate_configuration:logging_directory(D)
	,(    \+ exists_directory(D)
	     ,debug(learning_rate_setup,'Did not find logging directory ~w',[D])
	 ->   make_directory_path(D)
	     ,debug(learning_rate_setup,'Created logging directory ~w',[D])
	 ;    true
	 ).


%!	init_plot_dir is det.
%
%	Create the plotting directory if it does not exist.
%
init_plot_dir:-
	learning_rate_configuration:plotting_directory(D)
	,(    \+ exists_directory(D)
	     ,debug(learning_rate_setup,'Did not find plotting directory ~w',[D])
	 ->   make_directory_path(D)
	     ,debug(learning_rate_setup,'Created plotting directory ~w',[D])
	 ;    true
	 ).

:-initialization(init_log_dir, now).
:-initialization(init_plot_dir, now).


% ================================================================================
% Experiment logging
% ================================================================================

% Uncomment to echo logging to console
%:-debug(learning_rate).

% Uncomment to allow tracking progresss while logging to file.
:-debug(progress).

% Log learned hypotheses
%:-debug(learning_rate_full).

:-debug(learning_rate_setup).

%!	debug_timestamp(-Timestamp) is det.
%
%	Helper predicate to generate a Timestamp for log files.
%
%	The format of Timestamp in this predicate is suitable for naming
%	a file (i.e. no characters that give Windows the fits).
%
debug_timestamp(A):-
	get_time(T)
	,stamp_date_time(T, DT, local)
	,format_time(atom(A), '%d_%m_%y_%H_%M_%S', DT).


%!	start_logging(+Target) is det.
%
%	Start logging to a new log file.
%
start_logging(F/A):-
% initialize should work but causes run_learning_rate.pl to raise
% errors on the existence of the two directories. So we call the two
% initialisation goals again here.
	%initialize
	init_log_dir
	,init_plot_dir
	,configuration:learner(L)
	,learning_rate_configuration:logging_directory(D)
	,close_log(learning_rate)
	,debug_timestamp(T)
	,atomic_list_concat([L,learning_rate,F,A,T],'_',Bn)
	,file_name_extension(Bn,'.log',Fn)
	,directory_file_path(D,Fn,P)
	,open(P,write,S,[alias(learning_rate)])
	,debug(learning_rate>S)
	,debug(learning_rate_full>S).


%!	close_log(+Alias) is det.
%
%	Close the log file with the given Alias if it exists.
%
close_log(A):-
	(   is_stream(A)
	->  close(learning_rate)
	;   true
	).


%!	log_experiment_setup(+Target,+Limit,+Metric,+Steps,+Samples) is
%!	det.
%
%	Log configuration options and experiment parameters.
%
log_experiment_setup(T,L,M,K,Ss):-
	metric_name(M,N)
	,debug(learning_rate,'Experiment parameters:',[])
	,debug(learning_rate,'Target:  ~w',[T])
	,debug(learning_rate,'Metric:  ~w',[N])
	,debug(learning_rate,'Steps:   ~w',[K])
	,debug(learning_rate,'Samples: ~w',[Ss])
	,debug(learning_rate,'Time limit (sec): ~w',[L])
	,debug(learning_rate,'',[])
	,debug(learning_rate,'Configuration options:',[])
	,print_config(debug,learning_rate,all)
	,log_problem_statistics(T).


%!	log_problem_statistics(+Target) is det.
%
%	List statistics of the MIL problem for Target.
%
%	Currently this only lists the numbers of positive and negative
%	examples, background definitions and metarules in the initial
%	MIL problem for Target (i.e. before any automatic modifications
%	such as metarule extension).
%
log_problem_statistics(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
	,debug(learning_rate,'Positive examples:    ~w', [I])
	,debug(learning_rate,'Negative examples:    ~w', [J])
	,debug(learning_rate,'Background knowledge: ~w ~w', [K,BK])
	,debug(learning_rate,'Metarules:            ~w ~w ', [N,MS]).


%!	log_experiment_setup(+Metric,+Means,+SDs) is det.
%
%	Log experiment results. Er, duh.
%
log_experiment_results(M,Ms,SDs):-
	(   M = acc
	->  N = 'Accuracie'
	;   M = tpr
	->  N = 'True Positive Rate'
	;   metric_name(M,N)
	)
	,debug(learning_rate,'',[])
	,debug(learning_rate,'Mean ~ws: ~w',[N,Ms])
	,debug(learning_rate,'Standard deviations: ~w',[SDs]).


% ================================================================================
% Experiment code
% ================================================================================


%!	learning_rate(+Target,+Metric,+Steps,+Samples,-Means,-SDs) is
%!	det.
%
%	Perform a learning rate experiment and collect Means and SDs.
%
%	Target is a predicate indicator, the symbol and arity of the
%	learning target, so move/2 for grid world navigation.
%
%	Metric is an atom, the metric to be measured: err, acc, fpr,
%	etc.
%
%	Steps is an integer, the number of steps the experiment will run
%	for.
%
%	Samples is a list of numbers, either integers or floats. In each
%	Step, a learning atempt is made for each number in Samples.
%
%	Means is a list of length equal to Samples where each element is
%	the mean value of the selected Metric for each Sample size at
%	the same position in Samples.
%
%	SDs is a list of length equal to Samples storing the standard
%	deviations of the reasults averaged in Means.
%
learning_rate(T,M,K,Ss,Ms,SDs):-
	configuration:learner(Lr)
	,start_logging(T)
	,learning_rate_configuration:learning_rate_time_limit(L)
	,log_experiment_setup(T,L,M,K,Ss)
	,experiment_data(T,Pos,Neg,BK,MS)
	,learning_rate(T,Lr,L,[Pos,Neg,BK,MS],M,K,Ss,Rs)
	,(   Rs \= []
	 ->  true
	 ;   debug(learning_rate,'Empty learning rate result!',[])
	    ,print_message(warning,'Empty learning rate result!')
	 )
	,pairs_averages(Rs,Ms)
	,pairs_sd(Rs,Ms,SDs)
	% Print results to logging stream
	,log_experiment_results(M,Ms,SDs)
	% Print R vectors for plotting
	,print_r_vectors(T,M,Ss,Ms,SDs)
	,close_log(learning_rate).

%!	learning_rate(+Target,+Learner,+Limit,+Problem,+Metric,+Steps,+Samples,-Results)
%!	is det.
%
%	Business end of learning_rates/6.
%
%	Limit is the time limit set in learning_rate_time_limit/1.
%
%	Problem is a list [Pos,Neg,BK,MS] of the positive examples,
%	negative examples, background knowledge symbols and metarule
%	identifiers, respectively.
%
%	Results is a list of lists of length equal to Samples, where
%	each sub-list is the lits of values of the chosen Metric for
%	the corresponding Sample size.
%
learning_rate(T,Lr,L,[Pos,Neg,BK,MS],time,K,Ss,Rs):-
	!
	,findall(Vs
	       ,(between(1,K,J)
		,debug(progress,'~w:~w Step ~w of ~w',[Lr,T,J,K])
		,debug(learning_rate_full,'~w:~w Step ~w of ~w',[Lr,T,J,K])
		,findall(S-D
			,(member(S,Ss)
			 ,debug(progress,'~w:~w Sampling size: ~w',[Lr,T,S])
			 ,debug(learning_rate_full,'~w:~w Sampling size: ~w',[Lr,T,S])
			 ,evaluation:train_test_splits(S,Pos,Pos_Train,_Pos_Test)
			 ,evaluation:train_test_splits(S,Neg,Neg_Train,_Neg_Test)
			 % Learn timing soft-cuts learning predicate goal
			 % avoiding backtracking over nondet learning predicates.
			 ,learn_timing([Pos_Train,Neg_Train,BK,MS],L,Ps,D)
			 ,debug_clauses(learning_rate_full,'Learned:',Ps)
			 ,debug(learning_rate_full,'Duration: ~w sec',[D])
			 ,length(Ps, N)
			 ,debug(learning_rate_full,'Hypothesis size: ~w',[N])
			 )
			,Vs)
		)
	       ,Rs).
learning_rate(T,Lr,L,[Pos,Neg,BK,MS],M,K,Ss,Rs):-
	findall(Vs
	       ,(between(1,K,J)
		,debug(progress,'~w:~w Step ~w of ~w',[Lr,T,J,K])
		,debug(learning_rate_full,'~w:~w Step ~w of ~w',[Lr,T,J,K])
		,findall(S-V
			,(member(S,Ss)
			 ,debug(progress,'~w:~w Sampling size: ~w',[Lr,T,S])
			 ,debug(learning_rate_full,'~w:~w Sampling size: ~w',[Lr,T,S])
			 % Soft cut stops backtracking into multiple learning steps.
			 % when training with Thelma or with reduction(subhypothesis)
			 ,once(timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V))
			 ,debug_clauses(learning_rate_full,'Learned:',Ps)
			 ,debug(learning_rate_full,'Measured ~w: ~w',[M,V])
			 ,length(Ps, N)
			 ,debug(learning_rate_full,'Hypothesis size: ~w',[N])
			 )
			,Vs)
		)
	       ,Rs).


%!	learn_timig(+Problem,+Limit,-Program,-Time) is det.
%
%	Learn with a time limit and report the time it took.
%
%	Problem is a list [Pos,Neg,BK,MS], holding the elements of a MIL
%	problem.
%
%	Limit is a floating point number, the number of seconds to set
%	as a time limit for a learning attempt with Problem.
%
%	Program is the output of the current learning_predicate/1 given
%	Problem.
%
%	Time is a floating point number, the time, in seconds, that the
%	learning predicate took to complete one learning attempt on
%	Problem.
%
%	Note that Time is at most equal to Limit. If Limit is exceeded,
%	Time is equal to Limit and Program is empty.
%
learn_timing([Pos,Neg,BK,MS],L,Ps,T):-
	S is cputime
	,G = (   evaluation:learning_query(Pos,Neg,BK,MS,Ps)
	     ->  true
	     ;   Ps = []
	     )
	,C = call_with_time_limit(L,G)
	,R = debug(learning_curve_full,'Exceeded Time limit: ~w sec',[L])
	,catch(C,time_limit_exceeded,R)
	,E is cputime
	,T is E - S.



% ================================================================================
% R script data printing
% ================================================================================


%!	print_r_vectors(+Target,+Metric,+Means,+SDs) is det.
%
%	Print results as R vectors for plotting.
%
%	Results are printed to the debug stream and to the designated R
%	data file in r_data_file/1.
%
%	@tbd A part of what is written to the R data file is three lines
%	that translate between the vectors for errors, standard
%	deviations and sampling rates that are named after the learner,
%	to a learner-agnostic version, named e.g. learner.eval.mean etc.
%	This is meant to act as an interface between the two learners to
%	allow the R plotting script to be run by both, while at the same
%	time preserving in the R data file the information of the
%	learner's name that produced a set of results. Most likely of
%	course, the R data file itself will be overwritten
%	(intentionally or by mistake) and the same R vectors are
%	preserved in the simultaneously written log file anyway. But,
%	just in case we write them to the data file also. Having the
%	name of the learner in the R dataafile is also helpful
%	sometimes to quickly disambiguate between data files written
%	from the two learners.
%
print_r_vectors(T,M,Ss,Ms,SDs):-
	configuration:learner(L)
	,learning_rate_configuration:plotting_directory(D)
	,learning_rate_configuration:r_data_file(Bn)
	% Construct plotting file name
	,atomic_list_concat([L,Bn],'_',Bn_)
	,directory_file_path(D,Bn_,Fn)
	,metric_name(M,N)
	% Create R vectors of results and sampling rates
	,Ms_v =.. [c|Ms]
	,SDs_v =.. [c|SDs]
	,Ss_v =.. [c|Ss]
	% Format data as atoms for easier printing
	,format(atom(Ms_A),'~w.eval.mean <- ~w',[L,Ms_v])
	,format(atom(SDs_A),'~w.eval.sd <- ~w',[L,SDs_v])
	,format(atom(Ss_A),'~w.sampling.rates <- ~w',[L,Ss_v])
	,upcase_word(L,L_)
	,format(atom(L_A),'learner <- \'~w\'', [L_])
	,format(atom(T_A),'target <- \'~w\'', [T])
	,format(atom(N_A),'metric <- \'~w\'', [N])
	% Print to R data file and debug stream.
	,debug(learning_rate,'',[])
	,debug(learning_rate,'R data:',[])
	,open(Fn,write,S,[alias(r_data_file)])
	% Printing goal for call_cleanup/2.
	,G = (forall(member(V,[L_A
			     ,T_A
			     ,N_A
			     ,Ms_A
			     ,SDs_A
			     ,Ss_A])
		   ,print_or_debug(both,S/learning_rate,V)
		   )
	     % Transform named data to learner-generic data for plotting script
	     ,format(S,'learner.eval.mean <- ~w.eval.mean~n', [L])
	     ,format(S,'learner.eval.sd <- ~w.eval.sd~n', [L])
	     ,format(S,'learner.sampling.rates <- ~w.sampling.rates~n', [L])
	     )
	,C = close(S)
	,call_cleanup(G,C)
	,debug(learning_rate,'',[])
	,debug(learning_rate,'R data written to ~w',[Fn])
	,copy_scripts.


%!	metric_name(?Metric,?Name) is semidet.
%
%	A pretty Name for an evaluation Metric.
%
metric_name(acc,'Accuracy').
metric_name(err,'Error').
metric_name(fpr,'False Positive Rate').
metric_name(fnr,'False Negative Rate').
metric_name(tpr,'True Positive Rate (Recall)').
metric_name(tnr,'True Negative Rate').
metric_name(pre,'Precision').
metric_name(fsc,'F-Score').
metric_name(time,'Duration').


%!	upcase_word(+Word,-Upcased) is det.
%
%	Upcase the first letter of an atomic Word.
%
upcase_word(W,W_):-
	atom_chars(W,[C|Cs])
	,upcase_atom(C,U)
	,atom_chars(W_,[U|Cs]).


%!	copy_scripts is det.
%
%	Copy R plotting scripts to the plotting directory.
%
%	The source and destination paths for the copy operation are
%	taken from the learning rate configuration options
%	copy_plotting_scripts/1 and plotting_directory/1.
%
copy_scripts:-
	learning_rate_configuration:copy_plotting_scripts(false)
	,!.
copy_scripts:-
	learning_rate_configuration:copy_plotting_scripts(R)
	,learning_rate_configuration:plotting_directory(D)
	,absolute_file_name(R,R_)
	,copy_directory(R_,D)
	,debug(learning_rate,'R plotting scripts copied to ~w',[D]).

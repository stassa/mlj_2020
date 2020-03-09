:-module(auxiliaries, [learn/1
                      ,learn/5
                      ,experiment_data/5
                      ,learning_targets/1
                      ,cleanup_experiment/0
                      ,write_metagol_dataset/3
                      ,metagol_data_file/4
                      ,built_in_or_library_predicate/1
                      ,closure/3
                      ,program/3
                      ,print_or_debug/3
                      ,debug_clauses/3
                      ,debug_clauses/2
                      ,print_clauses/2
                      ,print_clauses/1
                      ,debug_config/1
                      ,list_config/0
                      ,print_config/3
                      ,set_configuration_option/2
                      ,assert_program/3
                      ,erase_program_clauses/1
                      ]).

:-use_module(metagol).
:-use_module(project_root(configuration)).
:-use_module(lib(term_utilities/term_utilities)).

/** <module> Compatibility layer between Louise and Metagol.

This module is named auxiliaries to ensure compatibility with experiment
code using predicates originally defined in the module src(auxiliaires)
in Louise.
*/

% ================================================================================
% Learning predicate auxiliaries
% ================================================================================
% Predicates mapping between Louise and Metagol learning predicates.


learn(T):-
        experiment_data(T,Pos,Neg,BK,MS)
        ,learn(Pos,Neg,BK,MS,Ps)
        ,print_clauses(Ps).



%!      learn(+Pos,+Neg,+BK,+Metarules,-Program) is nondet.
%
%       Learn a Program from the elements of a MIL problem.
%
%       Pos, Neg are lists of positive and negative example atoms,
%       respectively. BK is a list of predicate indicators of background
%       predicates and Metarules a list of metarule names, for a MIL
%       problem. Program is a program learned by Metagol from the
%       elements of the MIL problem.
%
%       Compatibility
%       -------------
%
%       The purpose of this predicate is to allow compatibility between
%       Metagol's learn/2 and learn/3 learning predicates and Louise's
%       learn/5 predicate, used in experiment code. learn/[2,3,5] are
%       declared multifile so that learn/5 can be implemented in terms
%       of learn/3 without touching the original Metagol code.
%
learn(Pos,Neg,_BK,_MS,Ps):-
        metagol:learn(Pos,Neg,Prog1)
        ,reverse(Prog1,Prog3),
        maplist(metagol:metasub_to_clause,Prog3,Ps).




% ================================================================================
% Experiment file auxiliaries
% ================================================================================
% Predicates to read, manipulate and transform Louise experiment files
% to Metagol training data.


%!      experiment_data(+Target,-Pos,-Neg,-BK,-Metarules) is det.
%
%       Collect the elements of a MIL problem for a Target predicate.
%
%       The elements of the MIL problem are collected from the Metagol
%       data file specified in the configuration option
%       metagol_data_file/1.
%
%       Target is the predicate indicator, F/A, of a learning target.
%
%       Pos and Neg are lists of atoms (unit clauses) representing
%       positive and negative examples of Target.
%
%       BK is a list of predicate indicators of background predicates
%       used to learned Target and defined in the specified Metagol
%       data file.
%
%       MS are the names of metarules used to learn Target and defined
%       in the specified Metagol data file.
%
%       Compatibility
%       -------------
%
%       Equivalent to Louise's experiment_data/5. This predicate is
%       responsible for collecting lists of positive and negative
%       example atoms (Pos and Neg, respectively), background knowledge
%       predicates' symbols and arities in S/A form (BK) and the
%       names (ids) of metarules known to the shim layer's
%       configuration (MS).
%
%       The purpose of this predicate in Louise is to collect all the
%       elements of a MIL problem before training can begin by calling
%       one of Louise's learning predicates, e.g. learn/5 (whose
%       arguments match the argumets of this predicate, except for
%       Target).
%
%       In Metagol, only the positive and negative examples' lists need
%       to be passed to a learning predicate, in particular learn/2 or
%       learn/3 whereas background predicates are marked by body_pred/1
%       clauses and metarules are defined directly in a Metagol data
%       file. However, for compatibility with Louise and with experiment
%       code, background predicates' indicators and metarule ids are
%       returned by this predicate.
%
experiment_data(T,_,_,_,_):-
	learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Pos,Neg,BK,MS):-
        configuration:metagol_data_file(P)
        ,user:ensure_loaded(P)
        ,findall(E
                ,user:pos(T,E)
                ,Pos_)
        ,findall(E
                ,user:neg(T,E)
                ,Neg_)
        ,maplist(sort,[Pos_,Neg_],[Pos,Neg])
        ,findall(F/A
                ,body_pred(F/A)
                ,BK)
        ,experiment_metarules(MS).


%!      experiment_metarules(-IDs) is det.
%
%       Collect the IDs of metarules in an experiment.
%
%       IDs is a list of atoms, the names given to known metarules in
%       named_metarule/2 clauses in the shim layer's configuration file.
%
%       This predicate finds all metarule/6 clauses in the dynamic
%       database and matches them to the second argument of
%       named_metarule/2 clauses in the configuration module.
%
%       The first argument of a named_metarule/2 clause is a metarule's
%       id (which is usually a metarule name from the MIL bibliography,
%       such as "chain" or "identity" etc) and its second argument is a
%       representation of that metarule in Metagol's user-level format,
%       as a metarule/3 atom of the form:
%
%       ==
%       metarule(Ss,Hs,Bs).
%       ==
%
%       Where Ss is a list of the metarule's second-order existentially
%       quantified variables, Hs is the head literal of the metarule in
%       list form, as in [Symbol,Arg1,...,Argn], and Bs is the body
%       literals of the metarule, also in list form.
%
%       metarule/3 atoms are expanded (via term_expansion/2) by Metagol
%       into into metarule/6 atoms. This expansion occurs when a Metagol
%       data file with user-provided metarules is loaded into Prolog. We
%       wish to match the user-provided metarules to their names
%       recorded in named_metarule/2, for record-keeping and for
%       compatibility with Louise. This predicate takes care of that
%       task.
%
experiment_metarules(MS):-
	findall(Id2
	       ,(metagol:metarule(Id1,Ss1,Hs1,Bs1,X1,Y1)
		,copy_term(metarule(Id1,Ss1,Hs1,Bs1,X1,Y1), M1)
		,numbervars(M1)
		,configuration:named_metarule(Id2,metarule(Ss2,Hs2,Bs2))
		% metarule/3 terms may be expanded to a list of terms
		% but the metarules in this set of experiments will probably
		% expand to a single term.
		,user:term_expansion(metarule(Ss2,Hs2,Bs2),[metagol:M2])
		,copy_term(M2, M2_)
		,numbervars(M2_)
		% Expanding a metarule/3 term assigns it a new Id that won't
		% match the Id of any metarule/6 term in the database even if
		% they are otherwise the same metarule.
		,nb_setarg(1,M2_,_)
		,M1 = M2_
		)
	       ,MS).



%!	learning_targets(+Targets) is det.
%
%	Collect learning Targets defined in an experiment file.
%
%	Targets is the list of predicate symbols and arities of each of
%	the target predicates that have background knowledge
%	declarations in background/2 clauses in the current experiment
%	file.
%
%	@tbd Changed from Louise definition to call
%	load_metagol_data/0 because we want to change
%	initialise_experiment/0 to actually, you know, initialise the
%	experiment (as in - write BK and metarules etc).
%
learning_targets(Ts):-
	load_metagol_data
        ,setof(T
              ,E^(user:pos(T, E))
              ,Ts).



%!	load_metagol_data is det.
%
%	Load the current experiment file into module user.
%
load_metagol_data:-
        configuration:metagol_data_file(P)
        ,user:ensure_loaded(P).



%!	cleanup_experiment is det.
%
%	Cleanup dynamic database after an experiment.
%
%       @tbd Currently, this does nothing at all and is in here only
%       because a cleanup_experiment/0 call is issued by
%       timed_train_and_test/7, the main predicate training and testing,
%       in the experiment code. Actually trying to cleanup by removing
%       clauses of predicates added to the dynamic database by metagol
%       seems to cause problems which I don't even want to try and
%       investigate far enough to document. The dynamic database is
%       evil and since Metagol doesn't provide a predicate to clean up
%       after itself, I'm just not writing one.
%
cleanup_experiment.
/* Seems to cause some unexpected behaviour.
cleanup_experiment_:-
	cleanup_dynamics.
*/


%!	cleanup_dynamics is det.
%
%	Cleanup dynamic Metagol predicates.
%
cleanup_dynamics:-
	configuration:dynamic_predicates(Ds)
	,forall(member(M:F/A,Ds)
	       ,(functor(T,F,A)
		,M:retractall(T)
		)
	       ).



%!      write_metagol_dataset(+Path,+Module,+Target) is det.
%
%       Write a Metagol dataset based on a Louise experiment File.
%
%       Path is the path to an experiment file in Louise's preferred
%       format. Module is the module name of the file in that Path.
%       Target is the predicate indicator of one of the learning targets
%       for which a MIL problem is fully specified in that Module.
%
%       This predicate is responsible for translating the experiment
%       file in the given Path to a data file in Metagol's preferred
%       format. Additionally, positive and negative examples are given
%       as clauses of pos/2 and negative/2, where the first argument in
%       each predicate is Target and the second argument an atom of
%       Target given as a positive or negative example.
%
%       Example:
%       ==
%       ?- write_metagol_dataset('../data/kinship/kin.pl',kin,kin/2).
%       ==
%
%       This will translate the Louise experiment file in
%       mlj_2020/data/kinship/kin.pl to a Metagol data file.
%
write_metagol_dataset(Ef,M,T):-
        use_module(Ef)
        ,louise_experiment_data(M,T,Pos,Neg,BK,MS)
        ,convert(bk,BK,BK_)
        ,convert(ms,MS,MS_)
	,convert(pos(T),Pos,Pos_)
	,convert(neg(T),Neg,Neg_)
	,collect_options(Os)
	,closure(BK,M,Cs)
	,metagol_data_file(M,D,_Fn,P)
	,make_directory_path(D)
	,S = open(P,write,Str,[alias(metagol_data_file)])
	,G = (write_terms(Str,Os)
	     ,format(Str,'~n',[])
	     ,forall(member(As,[BK_,MS_,Pos_,Neg_])
		    ,(write_terms(Str,As)
		     ,format(Str,'~n',[])
		     )
		    )
	     ,forall(member(Ps,Cs)
		    ,(write_terms(Str,Ps)
		     ,format(Str,'~n',[])
		     )
		    )
	     )
	,C = close(Str)
	,setup_call_cleanup(S,G,C).


% ! louise_experiment_data(+Module,+Target,-Pos,-Neg,-BK,-Metarules) is
% det.
louise_experiment_data(M,T,Pos,Neg,BK,MS):-
        findall(E
             ,M:positive_example(T,E)
             ,Pos_)
        ,findall(E
              ,M:negative_example(T,E)
              ,Neg_)
        ,maplist(sort,[Pos_,Neg_],[Pos,Neg])
        ,M:background_knowledge(T,BK)
        ,M:metarules(T,MS).


%!	convert(+Element,+Members,-Converted) is det.
%
%	Convert an Element of a MIL problem to Metagol's notation.
%
convert(bk,BK,BK_):-
	findall(body_pred(F/A)
	       ,member(F/A,BK)
	       ,BK_).
convert(ms,IDs,MS):-
	findall(M
	       ,(member(Id, IDs)
		,configuration:named_metarule(Id,M)
		)
	       ,MS).
convert(pos(T),Pos,Pos_):-
	findall(pos(T,E)
	       ,(member(E,Pos)
		)
	       ,Pos_).
convert(neg(T),Neg,Neg_):-
% Negative examples may be empty - this is handled in the conditional
% below.
        findall(neg(T,E)
               ,(member(E,Neg)
                )
               ,Neg_1)
        ,(   Neg_1 == []
         ->  Neg_ = [(neg('$VAR'('_'),'$VAR'('_')):- false)]
         ;   Neg_ = Neg_1
	 ).


%!	collect_options(+Options) is det.
%
%	Collect Metagol options to add to the Metagol dataset.
%
collect_options(Cs):-
	metagol:max_clauses(Max_C)
	,metagol:min_clauses(Min_C)
	,metagol:max_inv_preds(Max_Inv)
	,Cs = [metagol:max_clauses(Max_C)
	      ,metagol:min_clauses(Min_C)
	      ,metagol:max_inv_preds(Max_Inv)
	      ].


%!	metagol_data_file(+Base,-Dir,-Name,-Path) is det.
%
%	Create the file name and path for a metagol data file.
%
metagol_data_file(Bn,D_abs,Fn,P):-
	configuration:metagol_data_directory(D)
	,absolute_file_name(D,D_abs)
	,file_name_extension(Bn,'.pl',Fn)
	,directory_file_path(D_abs,Fn,P).


%!	write_terms(+Stream,+Terms) is det.
%
%	Write a list of prolog Terms to a Stream.
%
write_terms(S,As):-
	forall(member(A,As)
	      ,(numbervars(A)
	       ,write_term(S,A,[fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ,quoted(true)
			       ]
			  )
	       )
	      ).




% ================================================================================
% Program auxiliaries
% ================================================================================
% Predicates for inspecting and collecting the clauses a program.


%!	built_in_or_library_predicate(+Predicate) is det.
%
%	True for a built-in or autoloaded Predicate.
%
%	Thin wrapper around predicate_property/2. Used to decide what
%	programs to collect with closure/3 and what programs to
%	encapsulate.
%
built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).



%!	closure(+Progam_Symbols,+Module,-Closure) is det.
%
%	Collect all clauses of a program and its Closure.
%
%	As program/3, but also collects the definitions of programs in
%	the closure of a progam.
%
%	Progam_Symbols is a list of predicate symbols and arities, F/A,
%	of clauses in a program. Closure is the set of definitions of
%	the Symbols in Program_Symbols, and the definitions of the
%	programs in the closure of each program in Program_Symbols.
%
%	Module is the definition module of each program in Closure, or
%	a module importing that module. To ensure each program in
%	Closure is accessible the best thing to do is to export
%	everything to the user module.
%
%	Example
%	-------
%	==
%	?- closure([ancestor/2],user,_Cs),forall(member(P,_Cs),print_clauses(P)).
%
%	ancestor(A,B):-parent(A,B).
%	ancestor(A,B):-parent(A,C),ancestor(C,B).
%	parent(A,B):-father(A,B).
%	parent(A,B):-mother(A,B).
%	father(stathis,kostas).
%	father(stefanos,dora).
%	father(kostas,stassa).
%	mother(alexandra,kostas).
%	mother(paraskevi,dora).
%	mother(dora,stassa).
%	true.
%	==
%
%	@tbd Why is closure/3 returning a list of lists rather than a
%	flat list? In mil_problem:encapsulated_bk/2 it's flattened after
%	being called. Why do we need to do that?
%
closure(Ss,M,Cs):-
	closure(Ss,[],_Ps,M,[],Cs_)
	,reverse(Cs_, Cs).

%!	closure(+Symbols,+Path_Acc,-Path,+Module,+Acc,-Closure) is det.
%
%	Business end of closure/3.
%
closure([],Ps,Ps,_M,Cs,Cs):-
	!.
closure([F/A|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	functor(S,F,A)
	,built_in_or_library_predicate(S)
	,!
	,closure(Ss,Ps_Acc,Ps_Bind,M,Acc,Bind).
closure([S|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	\+ memberchk(S,Ps_Acc)
	,!
	,program(S,M,Cs)
	,closure(Ss,[S|Ps_Acc],Ps_Acc_,M,[Cs|Acc],Acc_)
	,program_symbols(Cs,Ss_)
	,closure(Ss_,Ps_Acc_,Ps_Bind,M,Acc_,Bind).
closure([_S|Ss],Ps,Ps_Acc,M,Acc,Bind):-
	closure(Ss,Ps,Ps_Acc,M,Acc,Bind).


%!	program_symbols(+Program,-Symbols) is det.
%
%	Collect symbols of body literals in a Program.
%
program_symbols(Ps,Ss):-
	clauses_literals(Ps,Ls)
	,setof(F/A
	      ,L^Ls^(member(L,Ls)
		    ,functor(L,F,A)
		    )
	      ,Ss).



%!	program(+Symbols,+Module,-Program) is det.
%
%	Collect all clauses of a Program.
%
%	Symbols is the list of predicate indicators, F/A, of clauses in
%	Program.
%
%	Module is the definition module for Progam. This can be set to
%	user if the Program is not defined in a module.
%
%	Program is a list of all the clauses of the predicates in
%	Symbols.
%
%	@tbd This doesn't attempt to sort the list of Symbols to exclude
%	duplicates- if the same Symbol is passed in more than once, the
%	same definition will be included that many times in Programs.
%
program(F/A,M,Ps):-
	!
	,program([F/A],M,Ps).
program(Ss,M,Ps):-
	findall(P
	       ,(member(F/A,Ss)
		,functor(H,F,A)
		,M:clause(H,B)
		,(   B == true
		 ->  P = H
		 ;   P = (H:-B)
		 )
		)
	       ,Ps).




% ================================================================================
% Printing and debugging auxiliaries
% ================================================================================
% Predicates for printing to streams (including user_output and the
% current debug stream).


%!	print_or_debug(+Print_or_Debug,+Stream_or_Subject,+Atom) is
%!	det.
%
%	Print or debug an Atom.
%
%	Print_or_Debug can be one of: [print,debug,both]. If "print",
%	Stream_or_Subject should be the name or alias of a stream
%	and Atom is printed at that Stream. If "debug",
%	Stream_or_Subject should be a debug subject and Atom is printed
%	to the current debug stream, iff the specified subject is being
%	debugged. If "both", Stream_or_Subject should be a term Str/Sub,
%	where Str the name or alias of a stream and Sub the name of
%	debug topic; then Atom is printed to the specified stream and
%	also to the current debug topic if Sub is being debugged.
%
print_or_debug(debug,S,C):-
	debug(S,'~w',[C]).
print_or_debug(print,S,C):-
	format(S,'~w~n',[C]).
print_or_debug(both,Str/Sub,C):-
	print_or_debug(print,Str,C)
	,print_or_debug(debug,Sub,C).



%!	debug_clauses(+Topic,+Message,-Clauses) is det.
%
%	Log a Message followed by a set of Clauses.
%
debug_clauses(T,M,Cs):-
	debug(T,'~w',[M])
	,debug_clauses(T,Cs).


%!	debug_clauses(+Topic,+Clauses) is det.
%
%	Debug a list of Clauses if Topic is being debugged.
%
debug_clauses(T,[]):-
	!
	,debug(T,'[]',[]).
debug_clauses(T,L):-
	\+ is_list(L)
	,!
	,debug_clauses(T,[L]).
debug_clauses(T,Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,format(atom(A),'~W',[C_, [fullstop(true)
					 ,numbervars(true)
					 ,quoted(true)]
				    ])
	       ,debug(T,'~w',A)
	       )
	      ).



%!	print_clauses(+Message,-Clauses) is det.
%
%	Print a Message followed by a set of Clauses.
%
print_clauses(M,Cs):-
	format('~w~n',[M])
	,print_clauses(Cs).


%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
print_clauses([]):-
	!
	,writeln([]).
print_clauses(L):-
	\+ is_list(L)
	,!
	,print_clauses([L]).
print_clauses(Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,write_term(C_, [fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ,quoted(true)
			       ])
	       )
	      ).




% ================================================================================
% Configuration auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating configuration options.


%!	debug_config(+Subject) is det.
%
%	Log configuration options to the debug stream for Subject.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	logged.
%
debug_config(S):-
	print_config(debug,S,main).


%!	list_config is det.
%
%	Print configuration options to the console.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	printed.
%
list_config:-
	print_config(print,user_output,main).


%!	print_config(+Print_or_Debug,+Stream_or_Subject,+Scope) is det.
%
%	Print or debug current configuration options.
%
%	Print_or_Debug is one of [print,debug] which should be
%	self-explanatory.
%
%	Stream_or_Subject is either a stream alias or a debug subject,
%	depending on the value of Print_or_Debug.
%
%	Scope is one of [main,all]. If Scope is "main", only
%	configuration options whose implementation module is
%	"configuration" are printed. If Scope is "all", all
%	configuration options re-exported by configuration.pl are
%	printed, which includes options defined elsewhere, e.g.
%	configuration files of libraries that are re-exported by
%	configuration.pl to avoid cluttering it etc.
%
%	If Scope is "all" configuration options are prepended by the
%	name of their implementation module, to help identification.
%
%	If Scope is something other than "main" or "all", print_config/3
%	raised an existence error.
%
%	Configuration options are printed in alphabetical order, which
%	includes the alphabetical order of their implementation modules'
%	names.
%
print_config(T,S,Sc):-
	must_be(oneof([main,all]), Sc)
	,module_property(configuration, exports(Es))
	,findall(M:Opt_
		,(member(F/A,Es)
		 ,\+ memberchk(F, [named_metarule
				  ,metarule
				  ,metarule_constraints
				  ,dynamic_predicates
				  ])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(M))
		 ,call(configuration:Opt)
		 % Convert to list to sort by functor only.
		 % Standard order of terms also sorts by arity.
		 ,Opt =.. Opt_
		 )
		,Opts)
	% Sort alphabetically
	,sort(Opts, Opts_)
	,(   Sc = all
	 ->  true
	 ;   Sc = main
	 ->  Mod = configuration
	 )
	,forall(member(Mod:Opt, Opts_)
	       ,(Opt_ =.. Opt
		,(   Sc = all
		 ->  print_or_debug(T,S,Mod:Opt_)
		 ;   Sc = main
		 ->  print_or_debug(T,S,Opt_)
		 )
		)
	       ).



%!	set_configuration_option(+Option,+Value) is det.
%
%	Change the Value of a configuration Option.
%
%	Option is an atom, the name of a configuration option defined in
%	(or exported to) module configuration.
%
%	Value is the set of the arguments of Option. Iff Option has a
%	single argument, Value can be a single atomic constant.
%	Otherwise, it must be a list.
%
%	set_configuration_option/2 first retracts _all_ clauses of the
%	named Option, then asserts a new clause with the given Value.
%
%	Only configuration options declared as dynamic can be changed
%	using set_configuration_option/2. Attempting to change a static
%	configuration option will raise a permission error.
%
%	@tbd This predicate cannot change configuration options with
%	multiple clauses (or at least can't change any but their first
%	clause). Such functionality may or may not be necessary to add.
%
%	@bug Sorta bug, but if set_configuration_option/2 is used as
%	intended, at the start of an experiment file, to set a necessary
%	configuration option, the configuration option thus changed will
%	remain changed until the option is changed with
%	set_configuration_option/2 or by editing the configuration file.
%	And note that just reloading the configuration file will not
%	reset the option- it will just add an extra clause of it in the
%	database, which will often cause unepxected backtracking. This
%	may cause some confusion, for example when setting the value of
%	extend_metarules/1 to something other than false for one
%	experiment, which then of course affects subsequent experiments.
%	It happened to me, it could happen to you.
%
set_configuration_option(N, V):-
	atomic(V)
	,!
	,set_configuration_option(N,[V]).
set_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(configuration:T)
	,assert(configuration:T_).




% ================================================================================
% Database auxiliaries
% ================================================================================
% Predicates for manipulating the Prolog dynamic database.
% Remember: the dynamic database is evil.


%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).


assert_program(_,[],Rs,Rs):-
	!.
assert_program(M,[A|P],Acc,Bind):-
	copy_term(A,A_)
	,numbervars(A_)
	,clause(M:A_,true)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	copy_term(C,H:-B)
	,numbervars(H:-B)
	,clause(M:H,B)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	assert(M:C,Ref)
	,assert_program(M,P,[Ref|Acc],Bind).



%!	erase_program_clauses(-Clause_References) is det.
%
%	Erase a list of Clause_References from the dynamic database.
%
%	Clause_References is meant to be a list of references of a
%	program's clauses asserted to the dynamic database with
%	assert_program/3.
%
%	The purpose of this predicate is to allow a set of clauses
%	previously asserted by invoking assert_program/3 to be removed
%	from the dynamic database without stumbling over module scoping
%	that can be complicated when a predicate is declared in one
%	module and then clauses of it are added in another module.
%
%	For example, the following is what you should expect to see in
%	the dynamic database after a theory of father/2 is learned and
%	asserted in the dynamic database, while there is also background
%	knowledge of father/2:
%
%	==
%	% Example copied from Thelma.
%	[debug] [1]  ?- listing(thelma:father/2).
%	:- dynamic tiny_kinship:father/2.
%
%	tiny_kinship:father(stathis, kostas).
%	tiny_kinship:father(stefanos, dora).
%	tiny_kinship:father(kostas, stassa).
%	tiny_kinship:father(A, C) :-
%	    thelma:
%	    (   father_1(A, B),
%	        parent(B, C)
%	    ).
%
%	true.
%	==
%
%	This happens whenever new clauses of a previous defined
%	predicate are asserted in a different module than the
%	predicate's original implementation module. The reason we may
%	wish to do that is to create "multiple worlds" each with
%	different definitions of a predicate. For example, Thelma, where
%	the above example is taken from, asserts a learned hypothesis to
%	the dynamic database in order to test it against the negative
%	examples. However, the clauses of the learned hypothesis can get
%	mixed up with the examples of the target predicate. This creates
%	an unholy mess that is very fiddly to manage.
%	erase_program_clauses/1 helps a little, but, ultimately, one
%	must never forget that the dynamic database is evil.
%
erase_program_clauses([]):-
	!.
erase_program_clauses([Ref|Rs]):-
	erase(Ref)
	,erase_program_clauses(Rs).





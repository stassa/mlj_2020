:-module(auxiliaries, [write_metagol_dataset/1
		      ,print_or_debug/3
		      ,debug_config/1
		      ,list_config/0
		      ,print_config/3
		      ,metagol_data_file/4
		      ,set_configuration_option/2
		      ,assert_program/3
		      ,erase_program_clauses/1
		      ,experiment_data/5
		      ,cleanup_experiment/0
		      ,learning_targets/1
		      ,built_in_or_library_predicate/1
		      ,closure/3
		      ,debug_clauses/3
		      ,debug_clauses/2
		      ,print_clauses/2
		      ,print_clauses/1
		      ,program/3
		      ]).

:-use_module(project_root(configuration)).
:-use_module(lib(term_utilities/term_utilities)).

/** <module> Auxiliary predicates for Louise compatibility layer.

Predicates in this module are copied from Louise's auxiliary module, in
louise/src/auxiliaries.pl. Where a predicate has been modified to
accommodate Metagol, or a new version has been written from scratch,
this is noted in the documentation for that predicate.

*/


%!	write_metagol_dataset(+Target) is semidet.
%
%	Create a dataset for Target in Metagol's format.
%
%	Target should be a learning target defined in the currently
%	loaded experiment file. This predicate will create a new dataset
%	in Thelma's format, place it in a Prolog file (not a module) in
%	the location determined in metagol_data/1 and load it into
%	memory, ready for use.
%
%	@tbd The new metagol dataset will clobber any preexisting file
%	with the same name and path.
%
write_metagol_dataset(T):-
	experiment_file(_P,M)
	,experiment_data(T,Pos,Neg,BK,MS)
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
	findall(neg(T,E)
	       ,(member(E,Neg)
		)
	       ,Neg_).


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
	configuration:metagol_data(D)
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
% Printing auxiliaries
% ================================================================================
% Predicates for printing to streams (including user_output).


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




% ================================================================================
% Experiment file auxiliaries
% ================================================================================
% Auxiliaries for inspecting and manipulating experiment files.


%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Data about a Target theory from the current experiment file.
%
%	Target is the predicate indicator of the predicate to be
%	learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
%	@ Alias for experiment_data(T,Pos,Neg,BK,MS,true).
%
%	@tbd Modified from Louise to return negative examples without
%	":-" prefix not needed in Metagol.
%
experiment_data(T,Pos,Neg,BK,MS):-
	experiment_data(T,Pos,Neg,BK,MS,true).



%!	experiment_data(+Target,-Pos,-Neg,-BK,-MS,+Sorted) is det.
%
%	Data about a Target theory from the current experiment file.
%
%	As experiment_data/5 but allows Sorted to determine whether
%	examples are sorted.
%
%	Sorted is a boolean indicating whether the lists of
%	positive and negative examples should be sorted with sort/2 so
%	that they are in the standard order of terms and contain only
%	unique elements. Otherwise, Pos and Neg are collected in the
%	order in which they are generated by the positive and negative
%	examples generators in the current experiment file.
%
%	Use this predicate to collect a MIL problem for dynamic
%	learning. Use experiment_data/5 to collect the elements of a MIL
%	problem for learn/5.
%
%	@tbd This predicate is necessary because of dynamic learning,
%	that is sensitive to the ordering of positive examples in
%	particular. Other operations in Louise expect a sorted list of
%	examples.
%
experiment_data(T,_,_,_,_,_):-
	learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Pos,Neg,BK,MS,S):-
	configuration:experiment_file(P,M)
	,user:use_module(P)
	,findall(Ep
		,M:positive_example(T,Ep)
		,Pos_)
	,findall(En
		,M:negative_example(T,En)
		,Neg_)
	,(   S == true
	 ->  maplist(sort,[Pos_,Neg_],[Pos,Neg])
	 ;   S == false
	 ->  [Pos_,Neg_] = [Pos,Neg]
	 ;   format(atom(E),'Unknown "sorted" option: ~w',[S])
	    ,throw(E)
	 )
	,once(M:background_knowledge(T,BK))
	,once(M:metarules(T,MS_))
	,(   MS_ == [all]
	 ->  configuration_metarules(MS)
	 ;   MS = MS_
	 )
	,!.



%!	configuration_metarules(+Metarules) is det.
%
%	Collect the names of all Metarules defined in the configuration.
%
configuration_metarules(MS):-
	findall(Id
	       ,(configuration:current_predicate(named_metarule,H)
		,predicate_property(H, implementation_module(configuration))
		,H =.. [named_metarule,Id|_]
		,clause(H, _B)
		)
	       ,MS).



%!	cleanup_experiment is det.
%
%	Cleanup dynamic database after an experiment.
%
%	@tbd Modified from Louise definition. The Metagol version must
%	remove body_pred/2 and metarule/3 terms from the database.
%
cleanup_experiment:-
	configuration:experiment_file(P,_M)
	,cleanup_dynamics
	,user:unload_file(P).


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
%	load_experiment_file/0 because we want to change
%	initialise_experiment/0 to actually, you know, initialise the
%	experiment (as in - write BK and metarules etc).
%
learning_targets(Ts):-
	load_experiment_file
	,experiment_file(_P, M)
	,findall(T
		,M:background_knowledge(T, _BK)
		,Ts).



%!	load_experiment_file is det.
%
%	Load the current experiment file into module user.
%
load_experiment_file:-
	experiment_file(P,_M)
	,user:use_module(P).




% ================================================================================
% Program auxiliaries
% ================================================================================
% Predicates for inspecting a program.


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

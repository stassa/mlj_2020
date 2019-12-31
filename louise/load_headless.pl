:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(output)).
user:file_search_path(scripts, project_root(scripts)).
user:file_search_path(experiment_data, project_root('../data')).
user:file_search_path(experiments, project_root('../experiments')).
user:file_search_path(learning_rate, experiments(learning_rate)).

:-use_module(configuration).
:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(src(auxiliaries)).
:-use_module(lib(evaluation/evaluation)).
:-use_module(lib(sampling/sampling)).
:-use_module(src(dynamic_learning)).
:-use_module(src(metagen)).
:-use_module(src(examples_invention)).

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:- set_prolog_flag(stack_limit, 4_294_967_296).
%:-set_prolog_flag(stack_limit, 8_589_934_592).
%:-set_prolog_flag(stack_limit, 17_179_869_184).
:- set_prolog_flag(stack_limit, 2_147_483_648).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).

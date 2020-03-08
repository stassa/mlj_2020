:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).

:-doc_browser.

:-use_module(src(metagol)).
:-use_module(configuration).
:-use_module(src(auxiliaries)).

edit_files:-
        configuration:metagol_data_file(P)
        ,edit(load_project)
        ,edit(configuration)
        ,edit(src(metagol))
        ,edit(src(auxiliaries))
        ,edit('../experiments/learning_rate/scripts/learning_rate.pl')
        ,edit('../experiments/learning_rate/scripts/run_learning_rate.pl')
        ,edit('../experiments/learning_rate/scripts/learning_rate_configuration.pl')
        ,edit(P)
	.
:-edit_files.

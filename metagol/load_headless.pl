:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).

:-use_module(src(metagol)).
:-use_module(configuration).
:-use_module(src(auxiliaries)).

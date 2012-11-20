-module(rldp_change_log).

-export([change_log/2]).

change_log(Revision, Fd) ->
    {ok, NewTerms} = file:consult("rebar.config.lock"),
    OldTerms = old_file_as_terms(Revision, "rebar.config.lock"),
    Changes = changed_deps(OldTerms, NewTerms),
    log_changed_rel_version(Fd, changed_rel_version(Revision)),
    log_new_deps(Fd, Changes),
    log_removed_deps(Fd, Changes),
    log_changed_deps(Fd, Changes),
    ok.

log_changed_rel_version(Fd, {Name, _Old, New}) ->
    io:format(Fd, "~s ~s~n~n", [Name, New]).

log_changed_deps(Fd, Changes) ->
    case proplists:get_value(changed, Changes) of
        [] ->
            ok;
        Deps ->
            io:format(Fd, "~s~n~n", ["Updated deps:"]),
            [ log_changed_dep(Fd, D) || D <- Deps ]
    end.

log_changed_dep(Fd, {Dep, [Old, New]}) ->
    Cmd = iolist_to_string(["git log --format='    %ad %h %s' --date=short ",
                            Old, "..", New]),
    Dir = iolist_to_string(["deps/", atom_to_list(Dep)]),
    Log = rldp_util:cmd_in_dir(Cmd, Dir),
    case Log of
        %% if no loggable differences, ommit
        "" -> ok;
        _ ->
            io:format(Fd, "  ~s~n~s~n", [Dep, Log])
    end.

log_removed_deps(Fd, Changes) ->
    log_deps(Fd, removed, Changes, "Removed deps:").

log_new_deps(Fd, Changes) ->
    log_deps(Fd, added, Changes, "New deps:").

log_deps(Fd, Key, List, Label) ->
    case proplists:get_value(Key, List) of
        [] ->
            ok;
        Deps ->
            io:format(Fd, "~s~n~n", [Label]),
            [ io:format(Fd, "    ~s~n", [D]) || D <- Deps ],
            io:format(Fd, "~s", ["\n"])
    end,
    ok.

%% also need to build the sets of added deps and removed deps.
changed_deps(OldTerms, NewTerms) ->
    OldDeps = sets:from_list(dep_names(OldTerms)),
    NewDeps = sets:from_list(dep_names(NewTerms)),
    AddedDeps = sets:to_list(sets:subtract(NewDeps, OldDeps)),
    RemovedDeps = sets:to_list(sets:subtract(OldDeps, NewDeps)),
    ChangedDeps = version_changed_deps(OldTerms, NewTerms),
    [{added, AddedDeps}, {removed, RemovedDeps},
     {changed, ChangedDeps}].

%% Return simple list of dependency names given a proplist as results
%% from file:consult on a rebar.config file.
dep_names(Terms) ->
    [ Dep || {Dep, _, _} <- proplists:get_value(deps, Terms) ].

version_changed_deps(OldTerms, NewTerms) ->
    OldDeps = merge_deps(OldTerms, dict:new()),
    MergedDeps = merge_deps(NewTerms, OldDeps),
    [ D || D = {_, [A, B]} <- dict:to_list(MergedDeps), A =/= B ].

changed_rel_version(Rev) ->
    {ok, Terms} = file:consult("rel/reltool.config"),
    {NewName, NewVersion} = version_from_reltool_config(Terms),
    OldTerms = old_file_as_terms(Rev, "rel/reltool.config"),
    {_OldName, OldVersion} = version_from_reltool_config(OldTerms),
    {NewName, OldVersion, NewVersion}.

version_from_reltool_config(Terms) ->
    SysConfig = proplists:get_value(sys, Terms),
    {rel, Name, Version, _} = lists:keyfind(rel, 1, SysConfig),
    {Name, Version}.

merge_deps(Terms, Dict) ->
    Deps = proplists:get_value(deps, Terms),
    lists:foldl(fun({Dep, _, {git, _, {tag, Tag}}}, ADict) ->
                        dict:append(Dep, Tag, ADict);
                   ({Dep, _, {git, _, SHA}}, ADict) ->
                        dict:append(Dep, SHA, ADict)
                end, Dict, Deps).

old_file_as_terms(Rev, Name) ->
    Dest = rldp_util:mktemp_name([Name, "_"]),
    Cmd = iolist_to_string(["git show ", Rev, ":", Name, " > ", Dest]),
    os:cmd(Cmd),
    {ok, Terms} = file:consult(Dest),
    file:delete(Dest),
    Terms.

iolist_to_string(L) ->
    binary_to_list(iolist_to_binary(L)).

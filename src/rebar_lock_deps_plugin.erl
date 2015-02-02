%%% vim: set ts=4 sts=4 sw=4 et:

%% @author Seth Falcon
%% @copyright 2011 Seth Falcon
%% @doc lock-deps: Generate Locked Dependencies for Rebar
%%
%% The lock-deps command generates an alternate rebar.config file that
%% lists every dependency of a project and locks them at the git
%% revision found in the deps directory.
%%
%% Basic usage is:
%% ```
%% ./rebar lock-deps [ignore=...]
%% '''
%%
%% See the README.md file for details on how to use the script in your
%% build and notes on implementation.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
-module(rebar_lock_deps_plugin).
-author("Seth Falcon <seth@userprimary.net>").
-author("Yuri Lukyanov <y.snaky@gmail.com>").

-export([
         'bump-rel-version'/2,
         'commit-release'/2,
         'log-changed-deps'/2,
         'lock-deps'/2,
         'list-deps-versions'/2,
         'tag-release'/2
        ]).

-define(RELTOOL_CONFIG, "rel/reltool.config").
-define(RELX_CONFIG, "relx.config").

-ifdef(TEST).
-compile([export_all]).
-endif.

'lock-deps'(Config, _AppFile) ->
    run_on_base_dir(Config, fun lock_deps/1).

'list-deps-versions'(Config, _AppFile) ->
    run_on_base_dir(Config, fun list_deps_versions/1).

%% @doc Update the version field in `rel/reltool.config'. The version
%% is assumed to take the form of `X.Y.Z' where X, Y, and Z are
%% non-negative integers. This command accepts a `version' argument to
%% control the new version. When `version' is "patch" (default),
%% "minor", or "major", the current version is appropriately
%% incremented. If `version' is any other value, the specified value
%% is taken as the new version literal with no format checking.
'bump-rel-version'(Config, _AppFile) ->
    run_on_base_dir(Config, fun bump_rel_version/1).

'log-changed-deps'(Config, _AppFile) ->
    run_on_base_dir(Config, fun log_changed_deps/1).

'commit-release'(Config, _AppFile) ->
    run_on_base_dir(Config, fun commit_release/1).

'tag-release'(Config, _AppFile) ->
    run_on_base_dir(Config, fun tag_release/1).

run_on_base_dir(Config, Fun) ->
    case rebar_utils:processing_base_dir(Config) of
        true -> Fun(Config);
        false -> ok
    end.

lock_deps(Config) ->
    DepsDir = rebar_config:get(Config, deps_dir, "deps"),
    Ignores = string:tokens(rebar_config:get_global(Config, ignore, ""), ","),
    DepDirs = ordered_deps(Config, DepsDir),
    SubDirs = rebar_config:get(Config, sub_dirs, []),
    DepVersions = get_dep_versions(DepDirs),
    AllDeps = collect_deps(["."|DepDirs++SubDirs]),
    NewDeps = get_locked_deps(DepVersions, AllDeps, Ignores),
    NewConfig = rebar_config:get_global(Config,
        lock_config, "./rebar.config.lock"),
    write_rebar_lock("./rebar.config", NewConfig, NewDeps),
    io:format("wrote locked rebar config to: ~s~n", [NewConfig]),
    ok.

list_deps_versions(Config) ->
    DepsDir = rebar_config:get(Config, deps_dir, "deps"),
    Dirs = ordered_deps(Config, DepsDir),
    DepVersions = get_dep_versions(Dirs),
    lists:foreach(fun({Dep, Ver}) ->
                          io:format("~s ~s~n", [Ver, Dep]);
                     ({Dep, Ver, _Url}) ->
                          io:format("~s ~s~n", [Ver, Dep])
                  end, DepVersions),
    ok.

%% Create rebar dependency specs for each dep in `DepVersions' locked
%% to the SHA1 in `DepVersions'. Spec details are taken from
%% `AllDeps'. Dependencies, listed by name (atom) in `Ignores' are not
%% locked and the spec found in `AllDeps' is passed through.
%%
get_locked_deps(DepVersions, AllDeps, Ignores) ->
    IgnoreNames = [ list_to_atom(I) || I <- Ignores ],
    NewDeps = [ begin
                    DepSpec = lists:keyfind(Name, 1, AllDeps),
                    lock_dep(DepSpec, Sha, Url)
                end
                || {Name, Sha, Url} <- DepVersions,
                   lists:member(Name, IgnoreNames) =:= false ],
    IgnoreDeps0 = [ lists:keyfind(Name, 1, AllDeps) || Name <- IgnoreNames ],
    IgnoreDeps = [ D || D <- IgnoreDeps0, D =/= false ],
    io:format("locked ~b deps~n", [length(NewDeps)]),
    io:format("ignored ~b deps~n", [length(IgnoreDeps)]),
    IgnoreDeps ++ NewDeps.

%% Write a locked down rebar.config file to `NewPath' based on the
%% rebar.config file found at `OrigPath'. This does not check for or
%% otherwise handle a `rebar.config.script' file.
write_rebar_lock(OrigPath, NewPath, NewDeps) ->
    {ok, Orig} = file:consult(OrigPath),
    New = lists:keyreplace(deps, 1, Orig, {deps, NewDeps}),
    {ok, F} = file:open(NewPath, [write]),
    io:fwrite(F, "~s~n~n",
        ["%% THIS FILE IS GENERATED. DO NOT EDIT IT MANUALLY %%"]),
    [ io:fwrite(F, "~p.~n", [Item]) || Item <- New ],
    io:fwrite(F, "~s", ["\n"]),
    file:close(F),
    ok.

lock_dep({Name, _Version, {Git, _Url}}, Sha, Url) ->
    {Name, ".*", {Git, Url, Sha}};
lock_dep({Name, _Version, {Git, _Url}, Extra}, Sha, Url) ->
    {Name, ".*", {Git, Url, Sha}, Extra};
lock_dep({Name, Version, {Git, _Url, _Tag}}, Sha, Url) ->
    lock_dep({Name, Version, {Git, _Url}}, Sha, Url);
lock_dep({Name, Version, {Git, _Url, _Tag}, Extra}, Sha, Url) ->
    lock_dep({Name, Version, {Git, _Url}, Extra}, Sha, Url).

%% Find the git SHA1s of all the dependencies in `DepsDir' and return
%% as a list of {Name, Sha} tuples where Name is an atom and Sha is a
%% string.
get_dep_versions(Dirs) ->
    [ sha_for_project(D) || D <- Dirs ].

sha_for_project(Dir) ->
    ShaWithNewLine = rldp_util:cmd_in_dir("git rev-parse HEAD", Dir),
    UrlWithNewLine = rldp_util:cmd_in_dir("git config --get remote.origin.url", Dir),
    Sha = re:replace(ShaWithNewLine, "\n$", "", [{return, list}]),
    Url = re:replace(UrlWithNewLine, "\n$", "", [{return, list}]),
    {list_to_atom(filename:basename(Dir)), Sha, Url}.


ordered_deps(Config, Dir) ->
    AllDeps = read_all_deps(Config, Dir),
    OrderedDeps = order_deps(AllDeps),
    [ filename:join(Dir, D) || D <- OrderedDeps ].

order_deps(AllDeps) ->
    Top = proplists:get_value(top, AllDeps),
    order_deps(lists:reverse(Top), AllDeps, []).

order_deps([], _AllDeps, Acc) ->
    de_dup(Acc);
order_deps([Item|Rest], AllDeps, Acc) ->
    ItemDeps = proplists:get_value(Item, AllDeps),
    case ItemDeps of
        undefined ->
            io:format("missing dependency: ~p\nAllDeps: ~p~n", [Item, AllDeps]),
            erlang:error({missing_dep, Item});
        _ ->
            order_deps(lists:reverse(ItemDeps) ++ Rest, AllDeps, [Item | Acc])
    end.

read_all_deps(Config, Dir) ->
    TopDeps = rebar_config:get(Config, deps, []),
    Acc = [{top, dep_names(TopDeps)}],
    DepDirs = filelib:wildcard(filename:join(Dir, "*")),
    Acc ++ [
     {filename:basename(D), dep_names(extract_deps(D))}
     || D <- DepDirs ].

dep_names([{Name, _Version, _GitSpec} | T]) ->
    [erlang:atom_to_list(Name) | dep_names(T)];
dep_names([{Name, _Version, _GitSpec, _Extra} | T]) ->
    [erlang:atom_to_list(Name) | dep_names(T)];
dep_names([_Skip | T]) ->
    dep_names(T);
dep_names([]) ->
    [].

de_dup(AccIn) ->
    WithIndex = lists:zip(AccIn, lists:seq(1, length(AccIn))),
    UWithIndex = lists:usort(fun({A, _}, {B, _}) ->
                                     A =< B
                             end, WithIndex),
    Ans0 = lists:sort(fun({_, I1}, {_, I2}) ->
                              I1 =< I2
                      end, UWithIndex),
    [ V || {V, _} <- Ans0 ].

collect_deps(Dirs) ->
    %% Note that there may be duplicate entries
    lists:foldl(fun(Dir, Acc) ->
                        extract_deps(Dir) ++ Acc
                end, [], Dirs).

extract_deps(Dir) ->
    case rebar_config_exists(Dir) of
        true ->
            ConfigFile = Dir ++ "/rebar.config",
            {ok, Config} = rebar_config:consult_file(ConfigFile),
            case lists:keyfind(deps, 1, Config) of
                {deps, Deps} -> Deps;
                false -> []
            end;
        false -> []
    end.

rebar_config_exists(Dir) ->
    Plain = filename:join(Dir, "rebar.config"),
    Script = Plain ++ ".script",
    filelib:is_regular(Plain) orelse filelib:is_regular(Script).

bump_rel_version(Config) ->
    case filelib:is_file(?RELTOOL_CONFIG) of
        true ->
            RTConfig = read_reltool_config(),
            UserVersion = rebar_config:get_global(Config, version, undefined),
            SysConfig = proplists:get_value(sys, RTConfig),
            {rel, Rel, OldVersion, Apps} = lists:keyfind(rel, 1, SysConfig),
            NewVersion = new_version(UserVersion, OldVersion),
            NewRel = {rel, Rel, NewVersion, Apps},
            SysConfig1 = lists:keyreplace(rel, 1, SysConfig, NewRel),
            RTConfig1 = lists:keystore(sys, 1, RTConfig, {sys, SysConfig1}),
            write_reltool_config(RTConfig1),
            io:format("Bumped to version: ~p~n", [NewVersion]),
            ok;
        false ->
            bump_relx_config_version(Config)
    end.

bump_relx_config_version(Config) ->
    bump_relx_config_version(filelib:is_file(?RELX_CONFIG), Config).

bump_relx_config_version(true, Config) ->
    %% NOTE: this needs to be smarter because relx.config can specify
    %% more than one release. But for now, the common case.
    {ok, Relx} = file:consult(?RELX_CONFIG),
    {release, {RelName, OldVersion}, Goals} = lists:keyfind(release, 1, Relx),
    UserVersion = rebar_config:get_global(Config, version, undefined),
    NewVersion = new_version(UserVersion, OldVersion),
    NewRel = {release, {RelName, NewVersion}, Goals},
    NewRelx = lists:keyreplace(release, 1, Relx, NewRel),
    write_relx_config(NewRelx),
    io:format("Bumped to version: ~p~n", [NewVersion]),
    ok;
bump_relx_config_version(false, _Config) ->
    io:format(?RELTOOL_CONFIG ++ " not found~n"
              ?RELX_CONFIG ++ " not found~n").

%% Assume a version of `X.Y.Z'. Default behavior is to increment
%% Z. Incrementing the minor (Y) or major (Z) can be requesting by
%% providing the version argument as "minor" or "major",
%% respectively. If a value other than "major", "minor", or "patch" is
%% encountered, it is taken as the version literal without any
%% checking.
new_version(undefined, OldVersion) ->
    new_version("patch", OldVersion);
new_version("major", OldVersion) ->
    {Maj, _Min, _Pat} = parse_version(OldVersion),
    version_to_str({Maj + 1, 0, 0});
new_version("minor", OldVersion) ->
    {Maj, Min, _Pat} = parse_version(OldVersion),
    version_to_str({Maj, Min + 1, 0});
new_version("patch", OldVersion) ->
    {Maj, Min, Pat} = parse_version(OldVersion),
    version_to_str({Maj, Min, Pat + 1});
new_version(NewVersion, _OldVersion) ->
    NewVersion.

version_to_str({Maj, Min, Patch}) ->
    string:join([ integer_to_list(V) || V <- [Maj, Min, Patch] ], ".").

parse_version(Version) ->
    [Maj, Min, Pat] = [ list_to_integer(V)
                        || V <- re:split(Version, "\\.", [{return, list}]) ],
    {Maj, Min, Pat}.

read_reltool_config() ->
    {ok, Config} = file:consult(?RELTOOL_CONFIG),
    Config.

write_reltool_config(Config) ->
    {ok, F} = file:open(?RELTOOL_CONFIG, [write]),
    [ io:fwrite(F, "~p.~n", [Item]) || Item <- Config ],
    io:fwrite(F, "~s", ["\n"]),
    file:close(F).

write_relx_config(Config) ->
    {ok, F} = file:open(?RELX_CONFIG, [write]),
    [ io:fwrite(F, "~p.~n", [Item]) || Item <- Config ],
    io:fwrite(F, "~s", ["\n"]),
    file:close(F).

log_changed_deps(Config) ->
    Fd = case rebar_config:get_global(Config, log, undefined) of
             undefined ->
                 standard_io;
             FileName ->
                 {ok, IO} = file:open(FileName, [write]),
                 IO
         end,
    Rev = rebar_config:get_global(Config, rev, "HEAD"),
    rldp_change_log:change_log(Rev, Fd),
    case Fd of
        standard_io -> ok;
        _ -> file:close(Fd)
    end,
    ok.

commit_release(Config) ->
    Rev = rebar_config:get_global(Config, rev, "HEAD"),
    rldp_change_log:commit_release(Rev).

tag_release(_Config) ->
    rldp_change_log:tag_release().

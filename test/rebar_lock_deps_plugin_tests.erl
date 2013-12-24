-module(rebar_lock_deps_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

-define(rldp, rebar_lock_deps_plugin).

-define(EXPECT_ERCHEF,
        ["rebar_lock_deps_plugin",

         "mochiweb","webmachine","opscoderl_wm","envy", "chef_authn","chef_certgen",
         "rebar_vsn_plugin","erlware_commons","meck", "jiffy","ej","ibrowse","mini_s3",
         "bear","folsom","opscoderl_folsom","pooler", "mixer",
         "chef_objects",

         "ejson","oauth","couchbeam","epgsql","sqerl","edown", "stats_hero","depsolver",
         "chef_db",

         "gen_server2","rabbit_common", "amqp_client","gen_bunny",
         "chef_index",

         "bcrypt", "fast_log",
         "chef_wm",

         "eper"]).

ordered_deps_fake_erchef_test() ->
    Config = rebar_config:new("../test/fake_erchef/rebar.config"),
    AllDeps = ?rldp:ordered_deps(Config, "../test/fake_erchef/deps"),
    ExpectWithPath = [ filename:join("../test/fake_erchef/deps", D)
                       || D <- ?EXPECT_ERCHEF ],
    ?assertEqual(ExpectWithPath, AllDeps).

order_deps_test() ->
    Config = rebar_config:new("../test/fake_erchef/rebar.config"),
    TopDir = "../test/fake_erchef/deps",
    AllDeps = ?rldp:read_all_deps(Config, TopDir),
    ?assertEqual(?EXPECT_ERCHEF, ?rldp:order_deps(AllDeps)).

read_all_deps_test() ->
    ExpectDeps = [{top, ["rebar_lock_deps_plugin","chef_wm","eper"]},
                  {"amqp_client",["rabbit_common"]},
                  {"bcrypt",[]},
                  {"bear",[]},
                  {"chef_authn",["envy"]},
                  {"chef_certgen",[]},
                  {"chef_db",
                   ["chef_objects","couchbeam","meck","sqerl","stats_hero","ej",
                    "depsolver", "envy"]},
                  {"chef_index",["jiffy","ibrowse","ej","gen_bunny","envy"]},
                  {"chef_objects",
                   ["erlware_commons","meck","jiffy","ej","ibrowse","mini_s3",
                    "chef_authn", "opscoderl_folsom","pooler","mixer","envy"]},
                  {"chef_wm",
                   ["opscoderl_wm","chef_authn","chef_certgen","chef_db","chef_index",
                    "chef_objects","bcrypt","fast_log","mixer","stats_hero","folsom","envy"]},
                  {"couchbeam",["ejson","oauth","ibrowse"]},
                  {"depsolver",["erlware_commons"]},
                  {"edown",[]},
                  {"ej",[]},
                  {"ejson",["mochiweb"]},
                  {"envy",[]},
                  {"eper",[]},
                  {"epgsql",[]},
                  {"erlware_commons",["rebar_vsn_plugin"]},
                  {"fast_log",[]},
                  {"folsom",["bear","meck"]},
                  {"gen_bunny",["meck","amqp_client","rabbit_common"]},
                  {"gen_server2",[]},
                  {"ibrowse",[]},
                  {"jiffy",[]},
                  {"meck",[]},
                  {"mini_s3",["ibrowse","envy"]},
                  {"mixer",[]},
                  {"mochiweb",[]},
                  {"oauth",[]},
                  {"opscoderl_folsom",["folsom"]},
                  {"opscoderl_wm",["webmachine"]},
                  {"pooler",[]},
                  {"rabbit_common",["gen_server2"]},
                  {"rebar_lock_deps_plugin",[]},
                  {"rebar_vsn_plugin",[]},
                  {"sqerl",["epgsql","pooler","envy"]},
                  {"stats_hero",["meck","edown","envy"]},
                  {"webmachine",["mochiweb"]}],
    Config = rebar_config:new("../test/fake_erchef/rebar.config"),
    TopDir = "../test/fake_erchef/deps",
    AllDeps = ?rldp:read_all_deps(Config, TopDir),
    ?assertEqual(ExpectDeps, AllDeps).




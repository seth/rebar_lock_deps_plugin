-module(rldp_util).

-export([cmd_in_dir/2,
         mktemp_name/1]).

%% Run `Cmd' in directory `Dir'.
cmd_in_dir(Cmd, Dir) ->
    {ok, Value} = rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]),
    Value.

mktemp_name(Prefix) ->
    <<Int:32/big-unsigned-integer>> = crypto:rand_bytes(4),
    lists:flatten([Prefix, io_lib:format("~8.16.0b", [Int])]).

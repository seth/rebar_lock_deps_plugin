-module(rldp_util).

-export([cmd_in_dir/2,
         mktemp_name/1]).

%% Run `Cmd' in directory `Dir'.
cmd_in_dir(Cmd, Dir) ->
    {ok, StartDir} = file:get_cwd(),
    try
        ok = file:set_cwd(Dir),
        os:cmd(Cmd)
    after
        file:set_cwd(StartDir)
    end.

mktemp_name(Prefix) ->
    <<Int:32/big-unsigned-integer>> = crypto:rand_bytes(4),
    lists:flatten([Prefix, io_lib:format("~8.16.0b", [Int])]).

-module(clib_script).
-export([
        check_node/0,
        update_cluster_name/0,
        update_network/2,
        update_yfs_count/1,
        get_yfs_conf/0
    ]).

-include("uss_common.hrl").

check_node() ->
    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/check_node.sh|grep ERR", [Root]),
    Result = cclib_utils:cmd(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true  ->
            {ok, node()};
        false ->
            L = string:tokens(Result, "\n"),
            {error, [{check_node_error, [list_to_binary(X)||X<-L]}]}
    end.

update_cluster_name() ->
    Cluster_name = "YFS_" ++ lists:concat(string:tokens(cclib_uuid:uuid(), "-")),
    conf_replace("cluster_name .*$", "cluster_name " ++ Cluster_name ++ ";\n").

update_network(Network, Netmask) ->
    conf_replace("network .*$", "network " ++ Network ++ ";\n"),
    conf_replace("mask .*$", "mask " ++ Netmask ++ ";\n").

update_yfs_count(TupleList) ->
    _Nc60 = proplists:get_value(c60, TupleList),
    Nmds = proplists:get_value(mds, TupleList),
    Ncds = proplists:get_value(cds, TupleList),
    Ncli = proplists:get_value(client, TupleList),

    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/yfs_conf.sh -s count ~B ~B ~B",
        [Root, Nmds, Ncds, Ncli]),
    cclib_utils:cmd(Cmd).

get_yfs_conf() ->
    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/yfs_conf.sh -g", [Root]),
    Result = cclib_utils:cmd(Cmd),
    F = fun(Line) ->
        Fields = string:tokens(Line, ","),
        if
            length(Fields) =:= 3 ->
                #conf_item{
                    catagory = yfs,
                    key      = string:strip(lists:nth(1, Fields)),
                    doc      = string:strip(lists:nth(2, Fields)),
                    value    = string:strip(lists:nth(3, Fields))
                };
            true ->
                #conf_item{}
        end
    end,
    L = string:tokens(Result, "\n"),
    [R || #conf_item{key=K}=R <- [F(X) || X <- L], K=/=undefined].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conf_replace(RE, Replace) ->
    Path = filename:join([?YFS_PREFIX, "etc/yfs.conf"]),
    conf_replace(Path, RE, Replace).

conf_replace(Path, RE, Replace) ->
    case file:read_file(Path) of
        {ok, B} ->
            F = fun(X) ->
                case re:run(X, RE) of
                    {match, _} -> Replace;
                    nomatch -> X++"\n"
                end
            end,
            L0 = string:tokens(binary_to_list(B), "\n"),
            L1 = [F(X) || X <- L0],
            file:write_file(Path, list_to_binary(L1)),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

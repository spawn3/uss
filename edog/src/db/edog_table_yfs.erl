-module(edog_table_yfs).
-compile(export_all).

-include("edog_common.hrl").

-spec yfs_getservers(all | yfs_server_flag(), boolean()) -> false | list().
yfs_getservers(_Proc, IpOnly) ->
		Proc = atom_to_list(_Proc),
    NewProc = case Proc of all -> '_'; _ -> Proc end,
    F = fun() -> mnesia:match_object({yfs_t, {NewProc, '_', '_'}, '_', '_', '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Servers} ->
            case IpOnly of
                true  -> lists:usort([IP || #yfs_t{server={_, IP, _}} <- Servers]);
                false -> Servers
            end;
        {aborted, _Reason} ->
            false
    end.

yfs_dumpxml() ->
    case yfs_getservers(all, false) of
        Servers when is_list(Servers) ->
            yfs_dumpxml_1(Servers, "")
    end.

yfs_dumpxml_1([], Acc) ->
    Xml = io_lib:format(
        "<cluster>\n"
        "<yfs>\n"
        "~s"
        "</yfs>\n"
        "</cluster>", [Acc]),
    lists:flatten(Xml);
yfs_dumpxml_1([#yfs_t{server={Proc, IP, N}}|T], Acc) ->
    NewAcc = io_lib:format("~s<~w ip='~s' num='~B'/>\n", [Acc, Proc, IP, N]),
    yfs_dumpxml_1(T, NewAcc).


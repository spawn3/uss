-module(edog_fg).
-compile(export_all).

-include("edog_common.hrl").

-define(use_notify, true).
-ifdef(use_notify).

maptourl(_S0) ->
    _S1 = re:replace(_S0 , "error: " , ""    , [global , {return , list}])  ,
    _S2 = re:replace(_S1 , " "       , "%20" , [global , {return , list}])  ,
    _S3 = re:replace(_S2 , "/"       , "%2F" , [global , {return , list}])  ,
    _S4 = re:replace(_S3 , "\\?"     , "%3F" , [global , {return , list}])  ,
    _S5 = re:replace(_S4 , "%"       , "%25" , [global , {return , list}])  ,
    _S6 = re:replace(_S5 , "#"       , "%23" , [global , {return , list}])  ,
    _S7 = re:replace(_S6 , "\\&"     , "%26" , [global , {return , list}])  ,
    _S8 = re:replace(_S7 , "="       , "%3D" , [global , {return , list}]).

send({Flag, UUID, ReplyFlag, Reply} = _Msg) ->
    WSHost = edog_option:option_get(ws_ip),
    WSPort = edog_option:option_get(ws_port),
    F = fun() ->
        Url = case Reply of
            {ok, Res, Diff} ->
                case ReplyFlag of
                    new_pm_id ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true&~w=~s&port=~w&time=~w",
                            [WSHost, WSPort, Flag, UUID, ReplyFlag, element(1,Res), element(2,Res), Diff]);
                    _ ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true&~w=~w&time=~B",
                            [WSHost, WSPort, Flag, UUID, ReplyFlag, Res, Diff])
                end;
            {error, _Reason, Diff} when is_list(_Reason) ->
                S = maptourl(_Reason),
                io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=false&error=~s&time=~B",
                    [WSHost, WSPort, Flag, UUID, S, Diff]);
            {error, _Reason, Diff}  ->
                io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=false&error=~w&time=~B",
                    [WSHost, WSPort, Flag, UUID, _Reason, Diff]);
            {ok, Res} ->
                case ReplyFlag of
                    done ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true",
                            [WSHost, WSPort, Flag, UUID]);
                    new_pm_id ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true&~w=~s&port=~w",
                            [WSHost, WSPort, Flag, UUID, ReplyFlag, element(1,Res), element(2,Res)]);
                    remark ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true&~w=~s",
                            [WSHost, WSPort, Flag, UUID, ReplyFlag, Res]);
                    _ ->
                        io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=true&~w=~w",
                            [WSHost, WSPort, Flag, UUID, ReplyFlag, Res])
                end;
            {error, _Reason} when is_list(_Reason) ->
                S = maptourl(_Reason),
                io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=false&error=~s",
                    [WSHost, WSPort, Flag, UUID, S]);
            {error, _Reason}  ->
                io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=false&error=~w",
                    [WSHost, WSPort, Flag, UUID, _Reason]);
            _ ->
                io_lib:format("http://~s:~w/action_response?flag=~w&uuid=~s&done=false&error=~w",
                    [WSHost, WSPort, Flag, UUID, Reply])
        end,

        Url1 = lists:flatten(Url),
        ?INFO({_Msg, Url1}),
        case httpc:request(get, {Url1, []}, [{timeout, 3000}], []) of
            {ok, _} ->
                ok;
                %?INFO({result, Url1});
            _Other ->
                ?ERROR({_Other})
        end
    end,
    spawn(F).

-else.

send({Flag, UUID, ReplyFlag, Reply}) ->
    case Reply of
        {ok, Res} ->
            ?INFO({Flag, UUID, Res});
        {error, _Reason} ->
            ?ERROR(_Reason);
        _Other ->
            ?ERROR(_Other)
    end.

-endif.

-spec notify(#notify_spec{}) -> any().

notify(#notify_spec{op=pm_down, key=PmID, reply=Reply}) ->
    send({pm_down, PmID, remark, Reply});
notify(#notify_spec{op=pm_up, key=PmID, reply=Reply}) ->
    send({pm_up, PmID, remark, Reply});
notify(#notify_spec{op=vm_create, key=VmID, reply=Reply}) ->
    send({vm_create, VmID, done, Reply});
notify(#notify_spec{op=vm_update, key=VmID, reply=Reply}) ->
    send({vm_update, VmID, done, Reply});
notify(#notify_spec{op=vm_destroy, key=VmID, reply=Reply}) ->
    send({vm_destroy, VmID, done, Reply});
notify(#notify_spec{op=vm_start, key=VmID, reply=Reply}) ->
    send({vm_start, VmID, port, Reply});
notify(#notify_spec{op=vm_stop, key=VmID, reply=Reply}) ->
    send({vm_stop, VmID, done, Reply});
notify(#notify_spec{op=vm_pause, key=VmID, reply=Reply}) ->
    send({vm_pause, VmID, done, Reply});
notify(#notify_spec{op=vm_resume, key=VmID, reply=Reply}) ->
    send({vm_resume, VmID, done, Reply});
notify(#notify_spec{op=vm_migrate, key=VmID, reply=Reply}) ->
    send({vm_migrate, VmID, new_pm_id, Reply});
notify(NotifySpec) ->
    ?ERROR({unknown, NotifySpec}).

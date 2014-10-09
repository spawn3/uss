-module(cclib_ssh).
-compile(export_all).

-include("cclib.hrl").

script_dir() ->
    Root = code:lib_dir(?CCLIB),
    filename:join([Root, "script"]).

ssh_test_string(Host, User, Password) ->
    io_lib:format("bash ~s/ssh_nopass_test.sh ~s ~s ~s", [script_dir(), Host, User, Password]).

is_ssh(Ip, User, Password) ->
    Cmd = ssh_test_string(Ip, User, Password),
    case ?os_cmd_no_report(Cmd) of
        <<"true\n">> -> true;
        _Other -> false
    end.

ssh_enable_string(Host, _User, Password) ->
    io_lib:format("bash ~s/ssh.sh -a ~s ~s", [script_dir(), Host, Password]).

enable_ssh(Ip, User, Password) ->
    Cmd = ssh_enable_string(Ip, User, Password),
    ?os_cmd_no_report(Cmd),
    case is_ssh(Ip, User, Password) of
        true ->
            true;
        false ->
            %?ALERT_ERR(alert, enable_ssh, io_lib:format("~p@~p", [User, Ip])),
            false
    end.

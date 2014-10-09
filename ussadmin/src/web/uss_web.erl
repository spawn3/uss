-module(uss_web).
-export([
        start/1,
        test/2,
        info/2,
        to_xml/2
    ]).

-include("uss_common.hrl").

start(Port) ->
    Root = code:lib_dir(?APPLICATION),
    cclib_http:start(Port, "httpd_uss",
        filename:join([Root, "www"]),
        [?MODULE, io, sql, misc],
        []
    ).

%%----------------------------------------------------------------------------------
test(Env, Input) ->
    [
        "Content-type: text/html\r\n\r\n",
        "<HTML><HEAD><TITLE>Example</TITLE></HEAD>\r\n",
        "<BODY>\n",
        "<B>Environment中国:</B>", io_lib:format("~p", [Env]), "<BR>\n",
        "<B>Input:</B>", Input, "<BR>\n",
        "</BODY></HTML>\n"
    ].

info(_Env, _Input) ->
    Xml = uss_manager:info(),
    [
        "Content-type: text/xml\r\n\r\n",
        to_xml(Xml, "")
    ].

%%--------------------------------------------------------------------
to_xml([], Acc) ->
    io_lib:format(
        "<xml>\n"
        "~s"
        "</xml>", [Acc]);
to_xml([{K, V}|T], Acc) ->
    NewAcc = io_lib:format(
        "~s"
        "<item key='~w' value='~s' />\n", [Acc, K, V]),
    to_xml(T, NewAcc).

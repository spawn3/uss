-module(cclib_dbg).
-compile(export_all).

-include_lib("stdlib/include/ms_transform.hrl").

start() ->
    dbg:tracer(),
    dbg:p(all, [call]),
    ok.

start(Port) ->
    start([node()], Port).

start(NodeList, Port) ->
    PortFun = dbg:trace_port(ip, Port),
    lists:foreach(fun(Node) -> dbg:tracer(Node, port, PortFun) end, NodeList),
    dbg:p(all, [call]),

    trace_pattern(),

    ok.

trace_pattern() ->
    ok.

stop() ->
    dbg:stop().

tp(M) -> tp(M, '_', '_').

tp(M, F) -> tp(M, F, '_').

tp(M, F, A) ->
    Ms = dbg:fun2ms(fun(_) -> return_trace() end),
    dbg:tp(M, F, A, Ms).

tpl(M) -> tpl(M, '_', '_').

tpl(M, F) -> tpl(M, F, '_').

tpl(M, F, A) ->
    Ms = dbg:fun2ms(fun(_) -> return_trace() end),
    dbg:tpl(M, F, A, Ms).

ctpl(M) -> ctpl(M, '_', '_').

ctpl(M, F) -> ctpl(M, F, '_').

ctpl(M, F, A) -> dbg:ctpl(M, F, A).

start_cover() ->
    cover:start(),
    cover:compile_beam_directory("/sysy/yfs/edog/ebin").

analyse_to_file() ->
    lists:foreach(fun(M) -> cover:analyse_to_file(M, [html]) end, cover:modules()).

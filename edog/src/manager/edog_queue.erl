-module(edog_queue).
-compile(export_all).

-include("edog_common.hrl").

-define(NO_QUEUE, true).
-ifdef(NO_QUEUE).

init() -> ok.
push(_,_) -> ok.
pop(_) -> ok.
exists(_) -> false.
empty() -> true.

-else.

init() ->
    _ = ets:new(edog_queue, [set, named_table, public]).

push(VmId, Info) ->
    ets:insert(edog_queue, {VmId, Info}),
    ?INFO(ets:info(edog_queue)).

pop(VmId) ->
    % TODO
    edog_common:flush_msg(),
    ets:delete(edog_queue, VmId).

exists(VmId) ->
    case ets:lookup(edog_queue, VmId) of
        [] -> false;
        _ -> true
    end.

empty() ->
    case ets:info(edog_queue, size) of
        0 -> true;
        _ -> false
    end.

-endif.

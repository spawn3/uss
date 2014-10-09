-ifndef(__EDOG_DEBUG_HRL__).
-define(__EDOG_DEBUG_HRL__, true).

-ifdef(debug).

-define(REPLY_ERROR(Op, Key, Reason),
    begin
        edog_notify:notify(#notify_spec{op=Op, key=Key, reply={error, Reason}}),
        ?INFO({"REPLY", {error, Reason}}),
        {error, Reason}
    end).

-else.

-define(REPLY_ERROR(_Op, _Key, _Reason), {error, _Reason}).

-endif.

-endif.

-module(uss_fg).
-compile(export_all).

-include("uss_common.hrl").

post(Url, TupleList) ->
    Json = iolist_to_binary(cclib_mochijson2:encode(TupleList)),
    ?INFO_REPORT(Json),
    case httpc:request(post,
            {Url, [], "application/json", Json},
            [{timeout, 2000}],
            [{sync, true}, {body_format, binary}]
            %[{keep_alive_timeout, 0}]
        ) of
        {ok, {StatusLine, Head, Body}} ->
            io:format("~p~n", [{StatusLine, Head, Body}]);
            %?INFO_REPORT({StatusLine, Head, cclib_utils:to_binary(Body)});
        {error, Reason} ->
            ?ERROR_REPORT(Reason),
            {error, Reason}
    end.

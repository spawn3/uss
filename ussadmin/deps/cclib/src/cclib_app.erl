-module(cclib_app).
-compile(export_all).

-include("cclib.hrl").

ensure_started(App) ->
    %?TTY_INFO_REPORT({ensure_started, App}),
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, App2}} ->
            ensure_started(App2);
        Other ->
            ?TTY_INFO_REPORT(Other)
    end.

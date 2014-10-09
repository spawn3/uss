-ifndef(__CCLIB_PROFILE_HRL__).
-define(__CCLIB_PROFILE_HRL__, true).

-include("cclib.hrl").

-define(PROFILE_OPERATION(Action, T1, T2, Args),
    begin
        PROFILE_TIME_12__ = timer:now_diff(T2, T1) div 1000,
        ?INFO([profile, {action, Action}, {time, PROFILE_TIME_12__}, Args]),
        PROFILE_TIME_12__
    end).

-define(PROFILE_BEGIN(), PROFILE_T1__ = erlang:now()).

-define(PROFILE_END(Action),
    begin
        PROFILE_T2__ = erlang:now(),
        ?PROFILE_OPERATION(Action, PROFILE_T1__, PROFILE_T2__, [])
    end).

-define(PROFILE_END_DETAIL(Action, Args),
    begin
        PROFILE_T2__ = erlang:now(),
        ?PROFILE_OPERATION(Action, PROFILE_T1__, PROFILE_T2__, Args)
    end).

-endif.

-module(edogdb_backup).
-export([backup_db/0,
        do_backup_db/0]).
-compile(export_all).

-include("edogdb.hrl").

-define(APP_DATA, "/sysy/yfs/ussadmin/edog_runtime").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB backup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unconsult(File, Term) ->
    io:format("writing ~p to ~p~n", [Term, File]),
    {ok, S} = file:open(File, write),
    io:format(S, "~p.~n" ,[Term]),
    file:close(S).

backup_dir() ->
    ?APP_DATA ++ "/backup_db".

lastest_file() ->
    filename:join([backup_dir(), "lastest.txt"]).

get_lastest() ->
    file:consult(lastest_file()).

set_lastest(Time) ->
    unconsult(lastest_file(), [{time, Time}, {now, erlang:now()}]).

backup_db() ->
    filelib:ensure_dir(backup_dir()),
    try
        case get_lastest() of
            {ok, [[{time, Time}, {now, Lastest}]]} when is_list(Time), is_tuple(Lastest) ->
                Diff = timer:now_diff(erlang:now(), Lastest) div 1000000,
                case edogdb_config:db_backup_interval() of
                    Interval when Diff >= Interval ->
                        do_backup_db();
                    _ ->
                        ok
                end;
            _Other ->
                ?INFO(_Other),
                do_backup_db()
        end
    catch
        Class:Exception ->
            ?INFO({Class, Exception, erlang:get_stacktrace()}),
            do_backup_db()
    end.

%%
do_backup_db() ->
    Time = cclib_utils:time_string(),
    File = filename:join([backup_dir(), "db-" ++ Time]),
    filelib:ensure_dir(File),
    ?INFO({dump_to_textfile, File, Time}),
    mnesia:dump_to_textfile(File),
    set_lastest(Time).

do_recover_db() ->
    case get_lastest() of
        {ok, [[{time, Time}, {now, Lastest}]]} when is_list(Time), is_tuple(Lastest) ->
            do_recover_db("db-" ++ Time);
        Other ->
            ?ERROR(Other)
    end.

do_recover_db(Txt) ->
    File = filename:join([backup_dir(), Txt]),
    ?INFO({"recover from", File}),
    mnesia:load_textfile(File).

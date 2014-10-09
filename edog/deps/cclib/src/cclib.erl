-module(cclib).
-compile(export_all).

-include("cclib.hrl").

src_dir() ->
    filename:join([code:lib_dir(cclib), "../.."]).

script_dir() ->
    filename:join([src_dir(), "script"]).

data_dir() ->
    filename:join([code:lib_dir(cclib), "../../../edog_runtime"]).

db_dir() ->
    filename:join([data_dir(), "db"]).

conf_dir() ->
    filename:join([data_dir(), "conf"]).

log_dir() ->
    filename:join(["/var/log/ussadmin", "edog_runtime"]).

start_logger(true) ->
    filelib:ensure_dir("/var/log/ussadmin/edog_runtime/"),
    application:start(cclib),
    cclib_app:ensure_started(log4erl),
    Log = filename:join([cclib:src_dir(), "conf/l4e_manager.conf"]),
    log4erl:conf(Log),
    error_logger:add_report_handler(error_logger_log4erl_h),
    log4erl:info("=========================================="),
    log4erl:info("=================LOG4ERL=================="),
    log4erl:info("=========================================="),
    log4erl:info(Log);
start_logger(false) ->
    filelib:ensure_dir("/var/log/ussadmin/edog_runtime/"),
    application:start(cclib),
    cclib_app:ensure_started(log4erl),
    Log = filename:join([cclib:src_dir(), "conf/l4e_agent.conf"]),
    log4erl:conf(Log),
    error_logger:add_report_handler(error_logger_log4erl_h),
    log4erl:info("=========================================="),
    log4erl:info("=================LOG4ERL=================="),
    log4erl:info("=========================================="),
    log4erl:info(Log).

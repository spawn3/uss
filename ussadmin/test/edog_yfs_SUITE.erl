-module(edog_yfs_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("edog_common.hrl").

-define(SF1, "/sysy/yfs/c60/0/status/status").
-define(SF2, "/sysy/yfs/mds/0/status/status").

suite() ->
    [{timetrap, {seconds, 300}}].

init_per_suite(Config) ->
    %application:start(?APPLICATION),

    os:cmd("pkill -9 c60d"),
    os:cmd("pkill -9 yfs_mds"),
    os:cmd("pkill -9 yfs_cds"),

    %filelib:ensure_dir(?SF1),
    %filelib:ensure_dir(?SF2),

    %os:cmd(io_lib:format("echo running > ~s", [?SF1])),
    %os:cmd(io_lib:format("echo running > ~s", [?SF2])),

    Config.

end_per_suite(Config) ->
    %application:stop(?APPLICATION),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [my_test_case, ps_test, yfs_test].

groups() -> [].

yfs_test() -> [].
yfs_test(_Config) ->
    edog_yfs:cds_info(1),
    edog_yfs:cds_info(2),
    edog_yfs:cds_info(3),

    ok.

ps_test() -> [].
ps_test(_Config) ->
    edog_yfs:info(c60, 0),
    edog_yfs:info(mds, 1),
    edog_yfs:info(cds, 1),
    edog_yfs:info(cds, 2),
    edog_yfs:info(cds, 3),

    edog_yfs:ps_info(c60, 0),
    edog_yfs:ps_info(mds, 1),
    edog_yfs:ps_info(cds, 1),
    edog_yfs:ps_info(cds, 2),
    edog_yfs:ps_info(cds, 3),

    ok.

my_test_case() -> [].
my_test_case(_Config) ->
    %{error, running} = edog_yfs_status:get_status(?SF1),
    %{error, running} = edog_yfs_status:get_status(?SF2),

    {ok, {can_lock, _}} = edog_yfs_status:get_status({c60, 0}),
    %{ok, {can_lock, _}} = edog_yfs_status:get_status({mds, 1}),

    Ip = edog_common:get_ip(node()),

    ok = edog_yfs:start({Ip, c60, 0}),
    ok = edog_yfs:start({Ip, mds, 1}),
    ok = edog_yfs:start({Ip, cds, 1}),
    ok = edog_yfs:start({Ip, cds, 2}),
    ok = edog_yfs:start({Ip, cds, 3}),

    timer:sleep(30000),
    edog_yfs:service_scan(),

    timer:sleep(10000),

    {ok, {can_lock, _}} = edog_yfs_status:get_status({c60, 0}),
    {ok, {can_lock, _}} = edog_yfs_status:get_status({mds, 1}),
    {ok, {can_lock, _}} = edog_yfs_status:get_status({cds, 1}),
    {ok, {can_lock, _}} = edog_yfs_status:get_status({cds, 2}),
    {ok, {can_lock, _}} = edog_yfs_status:get_status({cds, 3}),

    ok.

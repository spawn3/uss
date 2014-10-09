-module(edog_mnesia_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("edog_common.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:start(?APPLICATION),
    mnesia:clear_table(uss_rack_t),
    mnesia:clear_table(uss_pm_t),
    Config.

end_per_suite(Config) ->
    application:stop(?APPLICATION),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    mnesia:clear_table(uss_option_t),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [my_test_case, rack_test, pm_test, yfs_test, option_test, vnet_test].

groups() -> [].

my_test_case() -> [].
my_test_case(_Config) ->
    [uss_rack_t, uss_pm_t, uss_yfs_t, uss_option_t, uss_queue_t] = edog_mnesia:get_all_tables(),

    _L = [
        {"192.168.1.1", mds, 0}, {"192.168.1.1", mds, 1},
        {"192.168.1.2", mds, 0}, {"192.168.1.2", mds, 1},
        {"192.168.1.3", mds, 0}, {"192.168.1.3", mds, 1}],

    edog_mnesia:yfs_load_services(mds, 1),
    edog_mnesia:yfs_load_services(mds, 2),
    edog_mnesia:yfs_load_services(mds, 3),
    edog_mnesia:yfs_load_services(mds, 4),
    edog_mnesia:yfs_load_services(mds, 5),

    ok.

rack_test() -> [].
rack_test(_Config) ->
    [] = edog_mnesia:show(uss_rack_t),

    ok = edog_mnesia:p_rack_initid(),
    1 = edog_mnesia:p_rack_newid(),
    2 = edog_mnesia:p_rack_newid(),
    3 = edog_mnesia:p_rack_newid(),

    ok = edog_mnesia:p_rack_initid(),
    {ok, Id1} = edog_mnesia:rack_add("Rack1"),
    {error, exists} = edog_mnesia:rack_add("Rack1"),

    {ok, Id2} = edog_mnesia:rack_add("Rack2"),

    1 = Id1,
    2 = Id2,

    {ok, _} = edog_mnesia:rack_info(Id1),
    {ok, _} = edog_mnesia:rack_info(Id2),

    {ok, ok} = edog_mnesia:rack_delete(Id1),
    {ok, ok} = edog_mnesia:rack_delete(Id2),

    [] = edog_mnesia:show(uss_rack_t),

    ok.

pm_test() -> [].
pm_test(_Config) ->
    [] = edog_mnesia:show(uss_pm_t),

    ok = edog_mnesia:p_pm_initid(),
    1 = edog_mnesia:p_pm_newid(),
    2 = edog_mnesia:p_pm_newid(),
    3 = edog_mnesia:p_pm_newid(),

    Ip1 = "192.168.1.101",
    Ip2 = "192.168.1.102",

    %edog_mnesia:rack_add(<<"rack2">>),

    %ok = edog_mnesia:p_pm_initid(),
    %{ok, Id1} = edog_mnesia:pm_add({1, Ip1}),
    %{ok, Id2} = edog_mnesia:pm_add({1, Ip2}),

    %1 = Id1,
    %2 = Id2,

    %{ok, _} = edog_mnesia:pm_info(Id1),
    %{ok, _} = edog_mnesia:pm_info(Id2),

    %edog_mnesia:select({<<"node">>, null, {struct, [{<<"id">>, 1}]}, null, null}),
    %edog_mnesia:select({<<"node">>, null, {struct, [{<<"id">>, 2}]}, null, null}),

    %{error, exists} = edog_mnesia:pm_add(Ip1),
    %{error, exists} = edog_mnesia:pm_add(Ip2),

    %{ok, ok} = edog_mnesia:pm_delete(Id1),
    %{ok, ok} = edog_mnesia:pm_delete(Id2),

    %[] = edog_mnesia:show(uss_pm_t),

    ok.

yfs_test() -> [].
yfs_test(_Config) ->
    Ip = edog_common:get_ip(node()),
    edog_mnesia:yfs_select({Ip, c60}),
    edog_mnesia:yfs_select({Ip, mds}),
    edog_mnesia:yfs_select({Ip, cds}),

    edog_mnesia:yfs_start(c60),
    edog_mnesia:yfs_start(mds),
    edog_mnesia:yfs_start(cds),

    edog_mnesia:yfs_stop(c60),
    edog_mnesia:yfs_stop(mds),
    edog_mnesia:yfs_stop(cds),

    edog_mnesia:p_delete(uss_pm_t, 1),
    edog_mnesia:p_delete(uss_pm_t, 2),

    ok.

option_test() -> [].
option_test(_Config) ->
    ok = edog_mnesia:option_set(key1, value1),
    ok = edog_mnesia:option_set(key2, value2),

    {ok, value1} = edog_mnesia:option_get(key1),
    {ok, value2} = edog_mnesia:option_get(key2),

    {ok, default} = edog_mnesia:option_get(key3, default),

    {ok, ok} = edog_mnesia:option_del(key1),
    {ok, ok} = edog_mnesia:option_del(key2),
    {ok, ok} = edog_mnesia:option_del(key3),

    [] = edog_mnesia:show(uss_option_t),
    ok.

vnet_test() ->
    [
        {require, vif}
        %{default_config, vif, {1, "192.168.1.201"}}
    ].

vnet_test(Config) ->
    {Vlan, Ip} = ct:get_config(vif),
    ct:print(default, "~p~n", [{Vlan, Ip}]),

    ok.

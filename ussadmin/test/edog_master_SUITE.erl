-module(edog_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("edog_common.hrl").

suite() ->
    [{timetrap, {seconds, 300}}].

init_per_suite(Config) ->
    application:start(?APPLICATION),
    mnesia:clear_table(uss_rack_t),
    mnesia:clear_table(uss_pm_t),
    mnesia:clear_table(uss_yfs_t),
    %edog_master:init_db(),
    Config.

end_per_suite(Config) ->
    application:stop(?APPLICATION),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %mnesia:clear_table(uss_yfs_t),
    edog_mnesia:option_set(opt_uss_status, 'off'),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [rack_test, pm_test, yfs_test, my_test_case].

groups() -> [].

rack_test() -> [].
rack_test(_Config) ->
    %edog_master:yfs_start(),
    %%edog_master:yfs_stop(),
    ok.

pm_test() -> [].
pm_test(_Config) ->
    Ip1 = edog_common:get_ip(node()),
    Ip2 = "192.168.1.202",

    %edog_mnesia:rack_add(<<"rack1">>),

    %{ok, Id1} = edog_master:pm_add({1, Ip1}),
    %{ok, Id2} = edog_master:pm_add({1, Ip2}),

    %_ = edog_master:pm_info(Id1),
    %_ = edog_master:pm_info(Id2),

    %{ok, ok} = edog_master:pm_delete(Id1),
    %{ok, ok} = edog_master:pm_delete(Id2),

    ok.

yfs_test() -> [].
yfs_test(_Config) ->
    mnesia:clear_table(uss_yfs_t),

    Ip1 = edog_common:get_ip(node()),
    Ip2 = "192.168.1.202",

    %edog_mnesia:rack_add(<<"rack1">>),

    %{ok, Id1} = edog_master:pm_add({1, Ip1}),
    %{ok, Id2} = edog_master:pm_add({1, Ip2}),

    %_ = edog_master:pm_info(Id1),
    %_ = edog_master:pm_info(Id2),

    Services = [
        {Ip1, c60, 0},
        {Ip1, mds, 1},
        {Ip1, cds, 1},
        {Ip1, cds, 2},
        {Ip1, cds, 3}
    ],

    %lists:map(fun edog_master:yfs_add/1, Services),

    %{ok, _} = edog_mnesia:yfs_select(c60),
    %{ok, _} = edog_mnesia:yfs_select(mds),
    %{ok, _} = edog_mnesia:yfs_select(cds),

    %edog_mnesia:yfs_count(c60),
    %edog_mnesia:yfs_count(mds),
    %edog_mnesia:yfs_count(cds),
    %edog_mnesia:yfs_count(proxy),

    %[Ip1] = edog_mnesia:yfs_services(all, true),
    %_ = edog_mnesia:yfs_services(all, false),

    %[] = edog_master:yfs_deploy(edog),

    %edog_master:yfs_start(),
    %edog_master:yfs_stop(),

    %lists:map(fun edog_master:yfs_start/1, Services),
    %lists:map(fun edog_master:yfs_stop/1, Services),

    %ok = edog_master:yfs_start(c60),
    %ok = edog_master:yfs_start(mds),
    %ok = edog_master:yfs_stop(mds),
    %ok = edog_master:yfs_stop(c60),

    %lists:map(fun edog_master:yfs_delete/1, Services),

    %0 = edog_mnesia:yfs_count(c60),
    %0 = edog_mnesia:yfs_count(mds),
    %0 = edog_mnesia:yfs_count(cds),
    %0 = edog_mnesia:yfs_count(proxy),

    %{ok, ok} = edog_master:pm_delete(Id1),
    %{ok, ok} = edog_master:pm_delete(Id2),

    %true = edog_master:yfs_cleanlog({Ip1, c60}),
    %true = edog_master:yfs_cleanlog({Ip1, mds}),
    %true = edog_master:yfs_cleanlog({Ip1, cds}),
    %true = edog_master:yfs_cleanlog({Ip1, proxy}),
    %true = edog_master:yfs_cleanlog({Ip1, ynfs}),
    %true = edog_master:yfs_cleanlog({Ip1, yftp}),
    %true = edog_master:yfs_cleanlog(Ip1),
    %ok = edog_master:yfs_cleanlog(),



    %[] = edog_mnesia:show(uss_pm_t),
    %[] = edog_mnesia:get_nodes(),
    %[] = edog_mnesia:show(uss_yfs_t),
    ok.

my_test_case() -> [].
my_test_case(_Config) ->
    edog_agent:connect_to_master(),
    true = edog_master:master_is_alive(),

    edog_master:hostnames(),

    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

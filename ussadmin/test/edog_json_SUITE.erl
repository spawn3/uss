-module(edog_json_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("edog_common.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:start(?APPLICATION),
    Config.

end_per_suite(Config) ->

    application:stop(?APPLICATION),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    mnesia:clear_table(uss_rack_t),
    mnesia:clear_table(uss_pm_t),
    edog_mnesia:p_rack_initid(),
    edog_mnesia:p_pm_initid(),

    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [my_test_case, pm_test, proplists_test, sql_test].

groups() -> [].

proplists_test() -> [].
proplists_test(_Config) ->
    L = [{k1, 0}, {k2}, k3, {k4, 0, 1}],
    0         = proplists:get_value(k1, L),
    undefined = proplists:get_value(k2, L),
    true      = proplists:get_value(k3, L),
    undefined = proplists:get_value(k4, L),
    undefined = proplists:get_value(k5, L),

    false = edog_json:is_tuplelist([]),
    false = edog_json:is_tuplelist([a]),
    false = edog_json:is_tuplelist(L),
    true  = edog_json:is_tuplelist([{k,v}]),

    ok.

pm_test() -> [].
pm_test(_Config) ->
    %edog_fg:post("http://localhost:9601/sql/insert", [
    %        {who, rack},
    %        {what, [{name, rack1}]}]),
    %edog_fg:post("http://localhost:9601/sql/select", [{who, rack}]),
    %edog_fg:post("http://localhost:9601/misc/moni", [{id,1}]),
    %edog_fg:post("http://localhost:9601/misc/syst", [{action,start}]),
    %edog_fg:post("http://localhost:9601/misc/syst", [{action,stop}]),

    ok.

my_test_case() -> [].
my_test_case(_Config) ->
    io:format("~p~n", [?record_to_json(uss_rack_t, #uss_rack_t{})]),
    io:format("~p~n", [?record_to_json(uss_pm_t, #uss_pm_t{})]),
    io:format("~p~n", [?record_to_json(uss_yfs_t, #uss_yfs_t{})]),
    io:format("~p~n", [?record_to_json(uss_option_t, #uss_option_t{})]),

    ok.

sql_test() -> [].
sql_test(_Config) ->
    Ip1 = edog_common:get_ip(node()),

    {ok, [{id, 1}]} = sql:do_insert([
            {<<"who">>, <<"rack">>},
            {<<"what">>, {struct, [{<<"name">>, "rack1"}]}}]),
    {ok, [{id, 2}]} = sql:do_insert([
            {<<"who">>, <<"rack">>},
            {<<"what">>, {struct, [{<<"name">>, "rack2"}]}}]),

    {ok, [{id, 1}]} = sql:do_insert([
            {<<"who">>, <<"node">>},
            {<<"what">>, {struct, [{<<"rack">>, 1}, {<<"ip">>, edog_common:to_binary(Ip1)}]}}]),

    sql:do_select([
            {<<"who">>, <<"rack">>},
            {<<"where">>, {struct, [{<<"id">>, 1}]}}
        ]),
    sql:do_select([
            {<<"who">>, <<"node">>},
            {<<"where">>, {struct, [{<<"id">>, 1}]}}
        ]),
    sql:do_select([
            {<<"who">>, <<"service">>},
            {<<"where">>, {struct, [{<<"id">>, 1}]}}
        ]),

    %misc:do_moni([{<<"id">>, 1}]),
    %misc:do_moni([{<<"id">>, 2}]),
    %misc:do_syst([{<<"action">>, <<"start">>}]),
    %misc:do_syst([{<<"action">>, <<"stop">>}]),

    ok.

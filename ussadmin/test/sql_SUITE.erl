-module(sql_SUITE).
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [my_test_case].

groups() -> [].

my_test_case() -> [].
my_test_case(_Config) ->
    edog_mnesia:p_rack_initid(),
    edog_mnesia:p_pm_initid(),

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

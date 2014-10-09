-module(edog_common_SUITE).
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> [my_test_case, mac_test, uuid_test, check_cmd_result_test].

groups() -> [].

check_cmd_result_test() -> [].
check_cmd_result_test(_Config) ->
    true = edog_common:check_cmd_result("", [""]),
    true = edog_common:check_cmd_result("", ["a",""]),

    ok.


my_test_case() -> [].
my_test_case(_Config) ->
    {ok, 1} = application:get_env(?APPLICATION, asmaster),
    {ok, 1} = application:get_env(?APPLICATION, asagent),

    'ussadmin@192.168.1.1' = edog_common:get_node("192.168.1.1"),
    <<"192.168.1.1">> = edog_common:get_ip('ussadmin@192.168.1.1'),
    'ussadmin' = edog_common:to_atom("ussadmin"),

    LocalNode = node(),
    LocalIp = edog_common:get_ip(node()),
    LocalNode = edog_common:get_node(LocalIp),

    true  = edog_common:lists_unique([1,2,3]),
    false = edog_common:lists_unique([1,2,2]),

    [] = edog_common:lists_intersection([1,2], [4,3]),
    [2] = edog_common:lists_intersection([1,2], [2,3]),

    edog_common:now_to_string(),
    edog_common:time_string(),
    edog_common:cmd("echo"),
    edog_common:cmd(node(), "echo"),

    ok.

uuid_test() -> [].
uuid_test(_Config) ->
    true = edog_uuid:is_uuid(edog_uuid:uuid()),
    true = edog_uuid:is_uuid(edog_uuid:uuid("")),
    %true = edog_uuid:is_uuid(edog_uuid:uuid("abc")),

    ok.

mac_test() -> [].
mac_test(_Config) ->
    true = edog_uuid:is_mac(edog_uuid:to_mac()),
    true = edog_uuid:is_mac(edog_uuid:to_mac("")),
    true = edog_uuid:is_mac(edog_uuid:to_mac("abc")),
    ok.

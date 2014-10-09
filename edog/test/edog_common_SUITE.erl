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

groups() -> [].

all() -> [
        conf_test,
        node_test,
        pgrep_test,
        my_test_case,
        uuid_test,
        mac_test,
        check_cmd_result_test].

conf_test() -> [].
conf_test(_Config) ->
    Kvm1 = edog_conf:bin_kvm(),
    {ok, Kvm1} = application:get_env(?APPLICATION, bin_kvm),

    ok.

node_test() -> [].
node_test(_Config) ->
    {ok, 1} = application:get_env(?APPLICATION, asmaster),
    {ok, 1} = application:get_env(?APPLICATION, asagent),

    'edog@192.168.1.1' = edog_common:get_node("192.168.1.1"),
    "192.168.1.1" = edog_common:get_ip('edog@192.168.1.1'),
    'edog' = edog_common:to_atom("edog"),

    LocalNode = node(),
    LocalIp = edog_common:get_ip(node()),
    LocalNode = edog_common:get_node(?MASTERNAME, LocalIp),
    SlaveNode = edog_common:get_node(?AGENTNAME, LocalIp),
    SlaveNode = edog_common:get_node(LocalIp),

    ok.

pgrep_test() -> [].
pgrep_test(_Config) ->
    [1] = edog_common:pgrep("init"),
    true = edog_common:is_process_exists("init"),

    [] = edog_common:pgrep("xxx"),
    false = edog_common:is_process_exists("xxx"),

    ok.

check_cmd_result_test() -> [].
check_cmd_result_test(_Config) ->
    true = edog_common:check_result("", [""]),
    true = edog_common:check_result("", ["a",""]),
    true = edog_common:check_result("a", ["a","b"]),

    ok.

my_test_case() -> [].
my_test_case(_Config) ->
    edog_common:now_to_string(),
    edog_common:time_string(),
    edog_common:cmd("echo"),
    edog_common:cmd(node(), "echo"),

    ok.

uuid_test() -> [].
uuid_test(_Config) ->
    true = edog_uuid:is_uuid(edog_uuid:uuid()),
    true = edog_uuid:is_uuid(edog_uuid:uuid("")),
    true = edog_uuid:is_uuid(edog_uuid:uuid("abc")),
    true = edog_uuid:is_uuid(edog_uuid:uuid(undefined)),

    ok.

mac_test() -> [].
mac_test(_Config) ->
    true = edog_uuid:is_mac(edog_uuid:to_mac()),
    true = edog_uuid:is_mac(edog_uuid:to_mac("")),
    true = edog_uuid:is_mac(edog_uuid:to_mac("abc")),

    ok.

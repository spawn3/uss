-module(cclib_uuid).
-export([to_mac/0, to_mac/1, is_mac/1]).
-export([is_uuid/1, uuid/0, uuid/1]).
-export([v4/0, to_string/0, to_string/1, get_parts/1]).

-include("cclib_const.hrl").

%% ----------------------------------------------------
is_uuid(Uuid) when is_list(Uuid) ->
    length(Uuid) =:= 36;
is_uuid(Uuid) when is_binary(Uuid) ->
    is_uuid(binary_to_list(Uuid)).

uuid() -> uuid([]).

uuid(Uuid) ->
    try is_uuid(Uuid) of
        true -> Uuid;
        false -> to_string()
    catch
        _:_ ->
            to_string()
    end.

v4() ->
    random:seed(erlang:now()),
    v4(random:uniform(1 bsl 48) - 1, random:uniform(1 bsl 12) - 1, random:uniform(1 bsl 32) - 1, random:uniform(1 bsl 30)).
    %% v4(random:uniform(pow(2, 48)) - 1, random:uniform(pow(2, 12)) - 1, random:uniform(pow(2, 32)) - 1, random:uniform(pow(2, 30)) - 1).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

to_string() ->
    to_string(v4()).

to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

%% 00:c0:a0:xx:xx:xx
to_mac() ->
    {T1, T2, T3} = erlang:now(),
    <<R1:8, R2:8, R3:8, R4:8, R5:8, R6:8>> = <<0:8, 14:4, 0:4, 10:4, 0:4, T1:8, T2:8, T3:8>>,
    lists:flatten(io_lib:format("~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
            [R1, R2, R3, R4, R5, R6])).

to_mac(Mac) ->
    try is_mac(Mac) of
        true -> Mac;
        false -> to_mac()
    catch
        _:_ ->
            to_mac()
    end.

is_mac(Mac) when is_list(Mac) ->
    length(Mac) =:= 17;
is_mac(Mac) when is_binary(Mac) ->
    is_mac(binary_to_list(Mac)).

%pow(X, Y) ->
%    trunc(math:pow(X,Y)).

%uuid() ->
%    crypto:start(),
%    <<I:160/integer>> = cyypto:sha(term_to_binary({make_ref(), now()})),
%    erlang:integer_to_list(I, 16).

%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(utils).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").


%for test
-define(Data,
	{
		[
		{bike1,
			[{year,"2003"},{color,"black"},{condition,"new"}],
    	[{name,
    			[{manufacturer,["Harley Davidsson"]},
    				{brandName,["XL1200C"]},
    				{additionalName,["Sportster"]}]},
				{own, []},
    		{engine,
    			["V-engine, 2-cylinders, 1200 cc"]},
    		{kind,["custom"]},
    		{drive,["belt"]}
			]},
		{bike2,
    	[{year,"2003"},{color,"black"},{condition,"new"}],
    	[{name,
    			[{manufacturer,["Harley Davidsson"]},
    				{brandName,["XL1200C"]},
    				{additionalName,["Sportster"]}]},
    		{engine,
    			["V-engine, 2-cylinders, 1200 cc"]},
    		{kind,["custom"]},
    		{drive,["belt"]}
			]}
		 ]
	}).

%%%

%--------------------------
to_xml(Content, Response_name)->
	NewBike = #xmlElement{name=list_to_atom(Response_name), content=lists:flatten([Content])},
	Export = xmerl:export_simple([NewBike], xmerl_xml),
	io:format("~s~n", [lists:flatten(Export)]).

%%%

gen_request_id(N)->
	Chars = "abcdefghigklmnopqrstuvwxyzABCEDFGHIJKLMNOPQRSTUVWXYZ0123456789-",
	F1 = fun()->
				I = random:uniform(string:len(Chars)),
				C = string:substr(Chars, I, 1),
				io:format("~s", [C])
        end,
				lists:flatten([F1() || _ <- lists:seq(1, N)]).

%%%
is_upper(Char) when Char >= $A, Char =< $Z ->
    true;
is_upper(_Char) ->
    false.

camelcase_to_underscore(Str) ->
    F = fun(X) ->
        case is_upper(X) of
            true  -> [$_, string:to_lower(X)];
            false -> X
        end
    end,
    string:strip(lists:flatten([F(X) || X <- Str]), left, $_).
%%%

test_to_xml()->
	Content_a = ?Data,
	Name_a = "test_name",
	to_xml(Content_a, Name_a).

test_gen_request_id()->
	A = gen_request_id(35),
	string:len(A).

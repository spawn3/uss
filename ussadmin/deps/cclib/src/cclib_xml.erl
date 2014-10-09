-module(cclib_xml).
-export([
        get_attr/2,
        parse_msg/1,
        parse_msg/2,
        xml_isvalid/1
    ]).

-include_lib("xmerl/include/xmerl.hrl").
-include("cclib.hrl").

xml_isvalid(Doc) ->
    try xmerl_scan:string(Doc) of
        {#xmlElement{}, _Rest} -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

parse_msg([], Acc) ->
    lists:reverse(Acc);
parse_msg([H|T], Acc) ->
    Result = parse_msg(H),
    parse_msg(T, [Result|Acc]).

parse_msg(#xmlElement{name=module, attributes=Attrs, content=Content}) ->
    PluginName = get_attr(Attrs, name),
    MsgList = parse_msg(Content, []),
    {dest, list_to_atom(PluginName), MsgList};
parse_msg(#xmlElement{name=dest, attributes=Attrs, content=[H|_T]}) ->
    Host = get_attr(Attrs, ip),
    Type = get_attr(Attrs, type),
    #xmlText{type=cdata, value=X} = H,
    {Host, Type, X};
parse_msg([]) ->
    [];
parse_msg(Doc) ->
    try xmerl_scan:string(Doc) of
        {E, _R} ->
            parse_msg(E)
    catch
        Error:Expr ->
            ?INFO_REPORT({Error, Expr}),
            {error, scan}
    end.

get_attr(Attrs, Name) ->
    case lists:keyfind(Name, 2, Attrs) of
        #xmlAttribute{value=Val} ->
            Val;
        false ->
            false
    end.

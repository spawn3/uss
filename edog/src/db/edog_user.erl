-module(edog_user).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

customer_create(#cust_t{cust_name=CustName,
        cust_id=CustId, cust_home=Home} = Cust) ->
    F = fun() ->
        Q = qlc:q([X || X <- mnesia:table(cust_t),
                X#cust_t.cust_name =:= CustName]),
        case qlc:e(Q) of
            [] ->
                case mnesia:read({cust_t, CustId}) of
                    [] ->
                        % TODO
                        edog_storage:customer_create(Home),
                        mnesia:write(Cust),
                        CustId;
                    [Cust] ->
                        mnesia:abort(exists)
                end;
            Custs when is_list(Custs) ->
                mnesia:abort(exists)
        end
    end,
    cclib_mnesia:transaction(F, true).

customer_update(#cust_t{cust_id=CustId} = NewCust) ->
    F = fun() ->
        case mnesia:read({cust_t, CustId}) of
            [#cust_t{}=OldCust] ->
                Cust = OldCust#cust_t{
                    %cust_name = NewCust#cust_t.cust_name,
                    company   = NewCust#cust_t.company,
                    address   = NewCust#cust_t.address,
                    contact   = NewCust#cust_t.contact,
                    telephone = NewCust#cust_t.telephone,
                    cellphone = NewCust#cust_t.cellphone,
                    email     = NewCust#cust_t.email
                },
                mnesia:write(Cust),
                CustId;
            [] ->
                mnesia:abort(noent)
        end
    end,
    cclib_mnesia:transaction(F, true).

customer_lock(CustId) ->
    F = fun() ->
        case mnesia:read({cust_t, CustId}) of
            [] ->
                mnesia:abort(noent);
            [Cust] ->
                Now = cclib_utils:now_to_integer(),
                mnesia:write(Cust#cust_t{locked=true, locktime=Now})
        end
    end,
    cclib_mnesia:transaction(F).

customer_unlock(CustId) ->
    F = fun() ->
        case mnesia:read({cust_t, CustId}) of
            [] ->
                mnesia:abort(noent);
            [#cust_t{locked=true} = Cust] ->
                mnesia:write(Cust#cust_t{locked=false, locktime=0});
            [#cust_t{}] ->
                ok
        end
    end,
    cclib_mnesia:transaction(F).

customer_destroy(CustId) ->
    customer_lock(CustId).

-module(cclib_pmap).
-compile(export_all).

pmap(F, L) ->
  S = self(),
  Pids = lists:map(fun(I) ->
    spawn(fun() -> do_fun(S, F, I) end)
  end, L),
  gather(Pids).

gather([H|T]) ->
  receive
    {H, Result} -> [Result|gather(T)]
  end;
gather([]) ->
  [].

do_fun(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

%% ----------------------------------------------------------------------
%%
%% ----------------------------------------------------------------------
parallel_map(F, L) ->
    Main = self(),
    lists:foreach(
        fun(Key) ->
            G = fun() -> Main ! {self(), Key, F(Key)} end,
            spawn(G)
        end, L),
    wait_here(length(L), []).

wait_here(0, Acc) ->
    Acc;
wait_here(N, Acc) ->
    receive
        {From, Key, Value} when is_pid(From) ->
            wait_here(N-1, [{Key, Value}|Acc]);
        _ ->
            wait_here(N, Acc)
    end.

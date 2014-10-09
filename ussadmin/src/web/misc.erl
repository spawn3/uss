-module(misc).
-export([
        moni/2,
        syst/2
    ]).

moni(Env, Input) -> sql:moni(Env, Input).

syst(Env, Input) -> sql:syst(Env, Input).

-module(cclib_bmap).
-compile(export_all).

-define(PERM_YFS_START, 0).
-define(PERM_YFS_STOP,  1).

% zero-based
set(B, N) ->
    B bor (1 bsl N).

clear(B, N) ->
    B band (bnot (1 bsl N)).

test(B, N) ->
    1 band (B bsr N).

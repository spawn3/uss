-ifndef(__CCLIB_CONST_HRL__).
-define(__CCLIB_CONST_HRL__, true).

-define(CCLIB, cclib).

-define(INVALID_UUID, []).
-define(ERR_ID,       -1).

-define(NODE_HAVE_NONE,  2#00000000).
-define(NODE_HAVE_C60,   2#00000001).
-define(NODE_HAVE_MDS,   2#00000010).
-define(NODE_HAVE_NFS,   2#00000100).
-define(NODE_HAVE_PROXY, 2#00001000).

-endif.

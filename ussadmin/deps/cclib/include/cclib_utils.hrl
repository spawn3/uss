-ifndef(__CCLIB_UTILS_HRL__).
-define(__CCLIB_UTILS_HRL__, true).

-define(to_kb(Bytes), ((Bytes) div 1024)).

-define(assert(Expr),
    if Expr -> ok;
       true -> error({?MODULE, ?LINE, Expr})
    end).

-define(assertEqual(Expr1, Expr2),
    if (Expr1) =:= (Expr2) -> true;
       true -> error({?MODULE, ?LINE, Expr1, Expr2})
    end).

-endif.

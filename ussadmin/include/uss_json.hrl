-ifndef(__USS_JSON_HRL__).
-define(__USS_JSON_HRL__, true).

-define(record_update(RecordTag, Term, RefTupleList),
    begin
        true = is_record(Term, RecordTag),
        list_to_tuple([RecordTag |
                [V || {_K,V} <- uss_json:lists_update(?simple_record_to_tuplelist(RecordTag, Term), RefTupleList)]
        ])
    end).

-endif.

-ifndef(__CCLIB_JSON_HRL__).
-define(__CCLIB_JSON_HRL__, true).

-define(simple_record_to_tuplelist(RecordTag, Term),
    lists:zip(record_info(fields, RecordTag), tl(tuple_to_list(Term)))
).

-define(record_to_json(MapF, RecordTag, Term),
    [{MapF(K), MapF(V)} || {K, V} <- ?simple_record_to_tuplelist(RecordTag, Term)]
).

-define(tuplelist_to_json(MapF, TupleList),
    [{MapF(K), MapF(V)} || {K, V} <- TupleList]
).

-endif.

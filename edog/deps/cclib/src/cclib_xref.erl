-module(cclib_xref).
-compile(export_all).

-define(NAME, ?MODULE).

start() ->
    xref:start(?NAME),
    xref:set_default(?NAME, [{verbose,true},{warnings,true}]),
    %xref:add_release(?NAME, code:lib_dir(), [{name, otp}]).
    Dir = code:lib_dir(cclib),
    LibraryPath = [
        filename:join([Dir, "ebin"]),
        filename:join([Dir, "../mochiweb/ebin"]),
        filename:join([Dir, "../webmachine/ebin"]) | code:get_path()],
    xref:set_library_path(?NAME, LibraryPath, []),
    add_application("deps/cclib", [{name, cclib}]),
    add_application("."),
    analyse_undefined_function_calls().


add_application(Dir) ->
    add_application(Dir, []).

add_application(Dir, Options) ->
    xref:add_application(?NAME, Dir, Options).

analyse_undefined_functions() ->
    analyse(undefined_functions).

analyse_undefined_function_calls() ->
    analyse(undefined_function_calls).

analyse_module_use(ModSpec) ->
    analyse({module_use, ModSpec}).

analyse(Analyse) ->
    analyse(Analyse, []).

analyse(Analyse, Options) ->
    xref:analyse(?NAME, Analyse, Options).

stop() ->
    xref:stop(?NAME).

i() ->
    xref:info(?NAME).

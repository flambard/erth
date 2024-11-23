-module(erth_tests).
-include_lib("eunit/include/eunit.hrl").

file_with_compiled_word_test() ->
    {ok, F} = file:open("test/erth_code/double_example.erth", [read]),
    [900] = erth:start(F).

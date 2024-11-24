-module(erth_builtin).

-export([lookup_word/1]).

%% Builtin words
-export([
    dup/1,
    minus/1,
    multiply/1,
    plus/1,
    print/1,
    show/1,
    swap/1
]).

lookup_word(Word) ->
    BuiltinWords = #{
        "-" => minus,
        "*" => multiply,
        "+" => plus,
        "dup" => dup,
        "swap" => swap,
        "print" => print,
        "show" => show
    },
    maps:get(Word, BuiltinWords, undefined).

%%%
%%% Builtin words
%%%

dup([A | Stack]) ->
    [A, A | Stack].

minus([A, B | Stack]) ->
    [A - B | Stack].

multiply([A, B | Stack]) ->
    [A * B | Stack].

plus([A, B | Stack]) ->
    [A + B | Stack].

print([Value | Stack]) ->
    io:format("~p~n", [Value]),
    Stack.

show(Stack) ->
    io:format("STACK:~n~n"),
    lists:foreach(fun(Value) -> io:format("  ~p~n", [Value]) end, Stack),
    io:format("______~n"),
    Stack.

swap([A, B | Stack]) ->
    [B, A | Stack].

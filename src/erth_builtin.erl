-module(erth_builtin).

-export([minus/1, multiply/1, plus/1, print/1, show/1]).

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

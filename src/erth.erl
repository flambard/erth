-module(erth).

-export([start/0, start/1]).

start() ->
    start(standard_io).

start(Stream) ->
    _Compiled = ets:new(compiled_words, [named_table, set, public]),
    top_level_loop(Stream, []).

top_level_loop(Stream, Stack) ->
    case get_next_word(Stream) of
        eof ->
            Stack;
        compile_mode ->
            {Word, CompiledWords} = compile_word(Stream),
            ets:insert(compiled_words, {Word, CompiledWords}),
            top_level_loop(Stream, Stack);
        Word ->
            top_level_loop(Stream, evaluate_word(Word, Stack))
    end.

evaluate_word(Word, Stack) ->
    case lookup_word(Word) of
        {compiled, CompiledFun} -> CompiledFun(Stack);
        {builtin, Module, Function} -> Module:Function(Stack);
        undefined -> [Word | Stack]
    end.

lookup_word(Word) ->
    case ets:lookup(compiled_words, Word) of
        [{_CompiledWord, Fun}] ->
            {compiled, Fun};
        [] ->
            case erth_builtin:lookup_word(Word) of
                undefined -> undefined;
                FunctionAtom -> {builtin, erth_builtin, FunctionAtom}
            end
    end.

compile_word(Stream) ->
    Word = get_next_word(Stream),
    CompiledWords = compile(Stream),
    {Word, CompiledWords}.

compile(Stream) ->
    compile(Stream, fun(Stack) -> Stack end).

compile(Stream, CompiledFun) ->
    case get_next_word(Stream) of
        execute_mode ->
            CompiledFun;
        Word ->
            WrappedFun = fun(Stack) -> evaluate_word(Word, CompiledFun(Stack)) end,
            compile(Stream, WrappedFun)
    end.

get_next_word(IoDevice) ->
    case io:fread(IoDevice, "erth> ", " ~s") of
        eof -> eof;
        {ok, []} -> get_next_word(IoDevice);
        {ok, [Word]} -> parse_word(Word)
    end.

parse_word(":") ->
    compile_mode;
parse_word(";") ->
    execute_mode;
parse_word(Word) ->
    case string:to_integer(Word) of
        {Integer, ""} ->
            Integer;
        {error, _Reason} ->
            Word
    end.

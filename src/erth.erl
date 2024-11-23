-module(erth).

-export([start/0, start/1]).

start() ->
    start(standard_io).

start(Stream) ->
    Builtin = ets:new(builtin_words, [named_table, set, public]),
    ets:insert(Builtin,
               [{"-", erth_builtin, minus},
                {"*", erth_builtin, multiply},
                {"+", erth_builtin, plus},
                {"print", erth_builtin, print},
                {"show", erth_builtin, show}]),
    _Compiled = ets:new(compiled_words, [named_table, set, public]),
    top_level_loop(Stream, []).

top_level_loop(Stream, Stack) ->
    UpdatedStack =
        case get_next_word(Stream) of
            compile_mode ->
                {Word, CompiledWords} = compile_word(Stream),
                ets:insert(compiled_words, {Word, CompiledWords}),
                Stack;
            Word ->
                evaluate_word(Word, Stack)
        end,
    top_level_loop(Stream, UpdatedStack).

execute_compiled_word({_CompiledWord, Words}, InitialStack) ->
    lists:foldl(fun evaluate_word/2, InitialStack, Words).

evaluate_word(Word, Stack) ->
    case ets:lookup(compiled_words, Word) of
        [CompiledWord] -> 
            execute_compiled_word(CompiledWord, Stack);
        [] -> 
            case ets:lookup(builtin_words, Word) of
                [{_BuiltinWord, Module, Function}] ->
                    Module:Function(Stack);
                [] ->
                    [Word | Stack]
            end
    end.

compile_word(Stream) ->
    Word = get_next_word(Stream),
    CompiledWords = compile(Stream),
    {Word, CompiledWords}.

compile(Stream) ->
    compile(Stream, []).

compile(Stream, CompiledWords) ->
    case get_next_word(Stream) of
        execute_mode ->
            lists:reverse(CompiledWords);
        Word ->
            compile(Stream, [Word | CompiledWords])
    end.

get_next_word(IoDevice) ->
    case io:fread(IoDevice, "erth> ", " ~s") of
        {ok, []} ->
            get_next_word(IoDevice);
        {ok, [Word]} ->
            parse_word(Word)
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

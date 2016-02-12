-module(test).
-export([run/0]).

run() ->
    {ok, FileInfo} = file:read_file_info("test.erl"),
    Bytes = element(2, FileInfo),
    prn(123),
    Bytes.

prn(Txt) ->
    case is_integer(Txt) of
        true ->
            io:format("~B~n", [Txt]);
        false ->
            io:format("~s~n", [Txt])
    end.


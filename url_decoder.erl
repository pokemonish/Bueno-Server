-module(url_decoder).
-export([decode/1]).
% Юникод меня победил. Не хочу не буду

decode(Url) ->
    LastPosition = byte_size(Url) - 1,

    loop(Url, LastPosition, []).

loop(Url, Pos, Res) ->
    case binary:at(Url, Pos) of
        Char when   (Char>=38 andalso Char=<59 )
             orelse (Char>=63 andalso Char=<91 )
             orelse (Char>=97 andalso Char=<122)
             orelse  Char==33 orelse  Char==36
             orelse  Char==61 orelse  Char==35
             orelse  Char==93 orelse  Char==126 ->
            NewRes = [Char| Res],
            case Pos - 1 of
                -1 ->
                    NewRes;
                NextPos ->
                    loop(Url, NextPos, NewRes)
            end;
        $% ->
            NewRes = [$%| Res],
            case Pos - 1 of
                -1 ->
                    NewRes;
                NextPos ->
                    loop(Url, NextPos, NewRes)
            end
    end.
-module(server1).
-export([run/0, listen_sock/1]).
% не допер, как жить без экспорта и с передачей параметра
% кодировки поломаны


-define(HEADER_TIMEOUT, 100000  ).
-define(DOCUMENT_ROOT , "static").
-define(FILE_INDEX    , "index.html").


run() -> 
    {ok, ListenSock} = gen_tcp:listen(5678, [binary, {packet, 0}, {active, false}]),
    loop(ListenSock).

loop(ListenSock) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn(server1, listen_sock, [Sock]),
    loop(ListenSock).

listen_sock(Sock) ->
    {ok, Header} = gen_tcp:recv(Sock, 0, ?HEADER_TIMEOUT),
    [Method, UglyPath| _] = binary:split(Header, [<<" ">>, <<"\r">>, <<"\n">>], [trim_all, global]),
    case Method of
        <<"GET">> ->
            prn("get");
        <<"HEAD">> ->
            prn("head")
    end,
    InsecurePath = ?DOCUMENT_ROOT ++ http_uri:decode(binary:bin_to_list(UglyPath)),
    NotVeryGoodPath = string:join(lists:subtract(string:tokens(InsecurePath,"/"),[".",".."]),"/"),
    case binary:last(UglyPath) of
        $/ ->
            Path = NotVeryGoodPath ++ "/" ++ ?FILE_INDEX;
        _ ->
            case filelib:is_file(NotVeryGoodPath) of
                true ->
                    Path = NotVeryGoodPath;
                false ->
                    Path = NotVeryGoodPath ++ "/" ++ ?FILE_INDEX
            end
    end,
    % * Date
    % * Server
    % * Content-Length
    % * Content-Type
    % * Connection
    % * Корректный Content-Type для: .html, .css, js, jpg, .jpeg, .png, .gif, .swf
    prn(Path),
    ok = gen_tcp:close(Sock).

prn(Txt) -> io:format("~s~n", [Txt]).
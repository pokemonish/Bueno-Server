-module(httpd).
-export([start/1, start/2, start/3, stop/1]).


-define(HEADER_TIMEOUT, 1000).
-define(FILE_INDEX    , "index.html").
-define(SERVER_ENDING , "Server: Bueno Server 3000\r\n"++ 
                        "Connection: close\r\n\r\n").
-define(NOPE          , "Nope").

start(Port) ->
    start(Port, "./static").

start(Port, DocumentRoot) ->
    start(Port, DocumentRoot, {127,0,0,1}).

start(Port, DocumentRoot, Ip) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {ip, Ip}]) of
        {ok, ListenSock} ->
            spawn(fun() -> listen_sock(ListenSock, DocumentRoot) end),
            {ok, ListenSock};
        {error, Reason} ->
            {error, Reason}
    end.

stop(ListenSock) ->
    gen_tcp:close(ListenSock).

listen_sock(ListenSock, DocumentRoot) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            spawn(fun() -> listen_sock(ListenSock, DocumentRoot) end),
            handle_connect(Sock, DocumentRoot);
        {error, closed} ->
            ok
    end.

handle_connect(Sock, DocumentRoot) ->
    try
        {ok, ReceiveHeader} = gen_tcp:recv(Sock, 0, ?HEADER_TIMEOUT),
        [Method, UglyPath| _] = binary:split(ReceiveHeader, [<<" ">>, <<"?">>, <<"\r">>, <<"\n">>], [trim_all, global]),
        case Method of
            <<"GET">> ->
                ForbiddenMethod = false,
                SendBody = true;
            <<"HEAD">> ->
                ForbiddenMethod = false,
                SendBody = false;
            _ ->
                ForbiddenMethod = true,
                SendBody = false
        end,

        case ForbiddenMethod of
            false ->
                InsecurePath = http_uri:decode(binary:bin_to_list(UglyPath)),
                VeryVeryNotGoodPath = string:join(lists:filter(fun(X) -> X/=".." andalso X/="." end, string:tokens(InsecurePath,"\\")),"\\"),
                NotVeryGoodPath = string:join(lists:filter(fun(X) -> X/=".." andalso X/="." end, string:tokens(VeryVeryNotGoodPath,"/")),"/"),
                case binary:last(UglyPath) of
                    $/ ->
                        Path = DocumentRoot ++ "/" ++ NotVeryGoodPath ++ "/" ++ ?FILE_INDEX,
                        UserWantsFolder = not filelib:is_file(Path);
                    _ ->
                        case filelib:is_file(DocumentRoot ++ "/" ++ NotVeryGoodPath) of
                            true ->
                                Path = DocumentRoot ++ "/" ++ NotVeryGoodPath;
                            false ->
                                Path = DocumentRoot ++ "/" ++ NotVeryGoodPath ++ "/" ++ ?FILE_INDEX
                        end,
                        UserWantsFolder = false
                end,
                io:fwrite(Path, []), io:fwrite("~n", []),

                case UserWantsFolder of
                    false ->
                        Date = get_date(),
                        ContentType = get_content_type(Path),

                        {FileExists, FileInfo} = file:read_file_info(Path),
                        case FileExists of
                            ok ->
                                ContentLength = element(2, FileInfo),
                                
                                SendHeader = "HTTP/1.1 200 OK\r\n" ++
                                             "Date: " ++ Date ++ "\r\n" ++
                                             "Content-Type: " ++ ContentType ++ "\r\n" ++
                                             "Content-Length: " ++ integer_to_list(ContentLength) ++ "\r\n" ++
                                             ?SERVER_ENDING,
                                gen_tcp:send(Sock, SendHeader),

                                if SendBody ->
                                    {ok, _} = file:sendfile(Path, Sock)
                                end;
                            error ->
                                SendHeader = "HTTP/1.1 404 OK\r\n" ++
                                             "Date: " ++ Date ++ "\r\n" ++
                                             ?SERVER_ENDING ++
                                             ?NOPE,
                                gen_tcp:send(Sock, SendHeader)
                        end;
                    true ->
                        SendHeader = "HTTP/1.1 403 OK\r\n" ++
                                     ?SERVER_ENDING,
                        gen_tcp:send(Sock, SendHeader)
                end;
            true ->
                SendHeader = "HTTP/1.1 405 OK\r\n" ++
                             ?SERVER_ENDING,
                gen_tcp:send(Sock, SendHeader)
        end,
        ok = gen_tcp:close(Sock)
    catch
        _:_ ->
            gen_tcp:close(Sock)
    end,
    ok.

get_date() ->
    {Date, {Hours, Minutes, Seconds}} = calendar:universal_time(),
    {Year, MonthNumber, Day} = Date,
    DayOfWeek = element(calendar:day_of_the_week(Date), {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}),
    Month = element(MonthNumber, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
    io_lib:format("~s, ~B ~s ~B ~2..0B:~2..0B:~2..0B GMT", [DayOfWeek, Day, Month, Year, Hours, Minutes, Seconds]).

get_content_type(Filename) ->
    case lists:last(string:tokens(Filename, ".")) of
        T when T == "html" orelse T == "htm" ->
            "text/html";
        "css" ->
            "text/css";
        "js" ->
            "application/javascript";
        T when T == "jpg" orelse T == "jpeg" ->
            "image/jpeg";
        "png" ->
            "image/png";
        "gif" ->
            "image/gif";
        "swf" ->
            "application/x-shockwave-flash";
        _ ->
            "application/octet-stream"
    end.




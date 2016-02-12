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
    [Method, UglyPath| _] = binary:split(Header, [<<" ">>, <<"?">>, <<"\r">>, <<"\n">>], [trim_all, global]),
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

    Date = get_date(),
    Server = "koko",
    ContentType = get_content_type(NotVeryGoodPath),
    Connection = "close",

    {ok, FileInfo} = file:read_file_info(Path),
    ContentLength = element(2, FileInfo),
    
    prn(Path),
    prn(Date),
    prn(ContentType),
    prn(ContentLength),
    % {ok, BytesSent} = file:sendfile(Path, Sock),

    ok = gen_tcp:close(Sock).

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

prn(Txt) ->
    case is_integer(Txt) of
        true ->
            io:format("~B~n", [Txt]);
        false ->
            io:format("~s~n", [Txt])
    end.



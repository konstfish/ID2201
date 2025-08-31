-module(rudy).
-export([start/1, start/2, stop/0]).
-import(gen_tcp, []).
-import(http, []).

start(Port) ->
  start(Port, 1).

start(Port, Workers) ->
  register(rudy, spawn(fun() -> init(Port, Workers) end)).

stop() ->
  rudy ! kill.

init(Port, Workers) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  % open socket, options get us the request as a string (list)
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      Handlers = create_handlers(Listen, Workers), % on socket open, pass to handler(s)
      receive
        kill ->
          close_handlers(Handlers),
          gen_tcp:close(Listen), % close listening socket
          ok
      end;
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end.

close_handlers([]) ->
  ok;
close_handlers([Handler|Handlers]) ->
  io:format("Sending kill to Handler ~p~n", [ Handler ]),
  Handler ! kill,
  close_handlers(Handlers).

create_handlers(_, 0) ->
  [];
create_handlers(Listen, N) ->
  io:format("rudy: info: creating handler ~w~n", [N]),
  PID = spawn(fun() -> handler(Listen) end),
  [PID | create_handlers(Listen, N-1)].

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client), % accept request, pass to request handler(s)
      handler(Listen);
    {error, closed} ->
      io:format("rudy: warn: handler closed"),
      warn;
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end.

request(Client) ->
  % connection established, read input & return as string,
  % 0 = length = no limit
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str), % http module to parse request string
      Response = reply(Request), % build a basic reply using parsed req data
      gen_tcp:send(Client, Response); % send back the reply
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client). % close connection

% very simple uri cleanup to cut out url params and hash
parse_uri([]) ->
  [];
parse_uri([$?|_]) ->
  [];
parse_uri([$#|_]) ->
  [];
parse_uri([C|Tail]) ->
  Rest = parse_uri(Tail),
  [C|Rest].

% gets the extension from a filename
ext(File) ->
  lists:last(string:split(File, ".", all)).

% directory listing incl. hyperlinks
build_directory_index(_, []) ->
  [];
build_directory_index(BasePath, [File|DirContents]) ->
  % io:format("~p ~p~n", [File, DirContents]),
  Seperator = case lists:suffix("/", BasePath) of
    true -> "";
    false -> "/"
  end,
  "<a href=\""++ string:prefix(BasePath, ".") ++ Seperator ++ File ++ "\">"++ File ++"</a></br>" ++ build_directory_index(BasePath, DirContents).

reply_fs_directory(URIClean) ->
  case file:list_dir(URIClean) of
    {ok, DirContents} ->
      % http:ok(string:join(DirContents, " "));
      http:ok(build_directory_index(URIClean, DirContents));
    _ ->
      http:internal_error("Unable to retrieve directory contents")
  end.

reply_fs_file(URIClean, Size, Gzip) ->
  case file:read_file(URIClean) of
      {ok, Contents} ->
        MIME = http:mime(ext(URIClean)),

        BaseHeaders = [http:construct_header("Content-Type", MIME)],

        case Gzip of
          true ->
            GzContents = zlib:gzip(Contents),
            GzSize = byte_size(GzContents),

            ResponseHeaders = BaseHeaders ++ [
                      http:construct_header("Content-Length", integer_to_list(GzSize)),
                      http:construct_header("Content-Encoding", "gzip")
                    ],
            
            http:ok(GzContents, ResponseHeaders);

          false ->
            ResponseHeaders = BaseHeaders ++ [http:construct_header("Content-Length", integer_to_list(Size))],
            http:ok(Contents, ResponseHeaders)
          end;

      {error, Error} ->
      http:internal_error(Error)
  end.

reply({{get, URI, _}, Headers, _}) ->
  io:format("rudy: info: ~p serving ~p~n", [self(), URI]),
  
  % cleanup URI
  URIClean = "." ++ parse_uri(URI),

  case file:read_file_info(URIClean) of
    {error,enoent} ->
      http:not_found();
    {ok, FileInfo} ->
      case FileInfo of
        {file_info, _, directory, _, _, _, _, _, _, _, _, _, _, _} ->
          reply_fs_directory(URIClean);
        {file_info, Size, regular, _, _, _, _, _, _, _, _, _, _, _} ->
          Gzip = http:header_accept_encoding(Headers),
          reply_fs_file(URIClean, Size, Gzip);
        _ ->
          http:internal_error("Issue parsing file")
      end
  end;
% baseline echoserver implementation
reply({{post, URI, _}, _, Body}) ->
  io:format("rudy: info: ~p serving echo ~p~n", [self(), URI]),
  timer:sleep(40),
  http:ok(URI ++ "\r\n" ++ Body).


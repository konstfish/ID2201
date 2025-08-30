-module(rudy).
-export([start/1, stop/0]).
-import(gen_tcp, []).
-import(http, []).

start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  % open socket, options get us the request as a string (list)
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen), % on socket open, pass to handler
      gen_tcp:close(Listen), % close listening socket
      ok;
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client); % accept request, pass to request handler
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end,
  handler(Listen).

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

% directory listing incl. hyperlinks
build_directory_index(BasePath, []) ->
  [];
build_directory_index(BasePath, [File|DirContents]) ->
  io:format("~p ~p~n", [File, DirContents]),
  "<a href=\""++ BasePath ++ "/" ++ File ++ "\">"++ File ++"</a></br>" ++ build_directory_index(BasePath, DirContents).

reply({{get, URI, _}, _, _}) ->
  io:format("rudy: info: serving ~p~n", [URI]),
  
  % cleanup URI
  URIClean = "." ++ parse_uri(URI),

  % TODO: proper error handling & headers returned
  % write func to check content type
  % write server error 500 return
  % make this entire thing multithreaded w/ a thread pool to handle files
  % make this killable using kill
  % measure baseline func for report
  % write report
  case file:read_file_info(URIClean) of
    {error,enoent} ->
      http:not_found();
    {ok, FileInfo} ->
      case FileInfo of
        {file_info, _, directory, _, _, _, _, _, _, _, _, _, _, _} ->
          case file:list_dir(URIClean) of
            {ok, DirContents} ->
              % http:ok(string:join(DirContents, " "));
              http:ok(build_directory_index(URIClean, DirContents));
            _ ->
              http:not_found()
          end;
        {file_info, Size, regular, _, _, _, _, _, _, _, _, _, _, _} ->
          case file:read_file(URIClean) of
            {ok, Contents} ->
              http:ok(Contents);
            {error, Error} ->
              http:not_found()
          end;
        _ ->
          http:not_found()
      end
  end.
  
% baseline reply implementation
%reply({{get, URI, _}, _, _}) ->
%  io:format("rudy: info: serving ~p~n", [URI]),
%  timer:sleep(40),
%  http:ok("out" ++ URI).


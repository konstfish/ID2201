-module(http).
-export([parse_request/1, ok/1, ok/2, not_found/0, get/1]).

% request parsing
parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

%% request line parsing
request_line([$G, $E, $T, 32 |R0]) -> % match input to "GET", 32 (" "), and then the rest of the Request-Line
  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [13,10|R3] = R2, % rest of R2 starting at \r\n
  {{get, URI, Ver}, R3}. % R3 as the rest of the request

request_uri([32|R0]) ->
  {[], R0};

request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0), % recurse until it hits SP
  {[C|Rest], R1}. % return the URI and the rest

%% http version parsing
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

%% header parsing
headers([13, 10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0), % parse header
  {Rest, R2} = headers(R1), % recurse to next header
  {[Header|Rest], R2}. % return set of headers & rest

% single header
header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}. 

%% body parsing
message_body(R) ->
  {R, []}.

% replies
ok(Body) ->
  ok(Body, []).

ok(Headers, Body) ->
  response_builder("HTTP/1.1", "200", Body, Headers).

not_found() ->
  response_builder("HTTP/1.1", "404 Not Found", [], "Not Found").

response_builder(Version, Status, Headers, Body) ->
  Version ++ " " ++ Status ++ "\r\n" ++
  "Server: rudy/0.1\r\n" ++
  string:join(Headers, "\r\n") ++
  "\r\n" ++ Body.

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".


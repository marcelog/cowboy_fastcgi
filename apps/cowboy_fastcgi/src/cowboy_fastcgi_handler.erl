%%% @doc Page handler.
%%%
%%% Copyright 2017 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(cowboy_fastcgi_handler).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/cowboy_fastcgi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc https://ninenines.eu/docs/en/cowboy/2.0/guide/handlers/
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, Opts) ->
  Script = binary_to_list(cowboy_req:binding(script, Req0)),

  {_MegaSecs, _Secs, MicroSecs} = os:timestamp(),

  RequestId = MicroSecs rem 65535,
  QueryString = cowboy_req:qs(Req0),
  Method = cowboy_req:method(Req0),
  ContentType = cowboy_req:header(<<"content-type">>, Req0, <<"text/html">>),
  ContentLength = cowboy_req:header(<<"content-length">>, Req0, 0),

  {PeerIp, PeerPort} = cowboy_req:peer(Req0),
  StringPeerIp = inet:ntoa(PeerIp),

  LocalName = cowboy_req:host(Req0),
  LocalPort = cowboy_req:port(Req0),
  Uri = cowboy_req:path(Req0),
  %% Only short bodies!
  {ok, Body, Req1} = cowboy_req:read_body(Req0),

  cowboy_fastcgi_sup:fastcgi_run(RequestId, [
    {"SCRIPT_FILENAME", filename:join([?CFG:php_fpm_root(), Script])},
    {"QUERY_STRING", binary_to_list(QueryString)},
    {"REQUEST_METHOD", binary_to_list(Method)},
    {"CONTENT_TYPE", binary_to_list(ContentType)},
    {"CONTENT_LENGTH", integer_to_list(ContentLength)},
    {"SCRIPT_NAME", Script},
    {"GATEWAY_INTERFACE", "CGI/1.1"},
    {"REQUEST_URI", binary_to_list(Uri)},
    {"REMOTE_ADDR", StringPeerIp},
    {"REMOTE_PORT", integer_to_list(PeerPort)},
    {"SERVER_ADDR", "127.0.0.1"},
    {"SERVER_PORT", integer_to_list(LocalPort)},
    {"SERVER_NAME", binary_to_list(LocalName)}
  ], Body),

  fastcgi_wait(Req1, Opts, false).

fastcgi_wait(Req0, Opts, StreamStarted) ->
  receive
    {fast_cgi_stdout, _Id, Data} ->
      lager:info("PHP-FPM data: ~p", [Data]),
      Req2 = case StreamStarted of
        true ->
          cowboy_req:stream_body(Data, nofin, Req0),
          Req0;
        false ->
          {RawHeaders, Body} = parse_headers_and_body(Data),
          Headers = parse_and_pack_headers(RawHeaders),
          #{<<"status">> := StatusBin} = Headers,
          Status = bin_to_http_status(StatusBin),
          Req1 = cowboy_req:stream_reply(Status, Headers, Req0),
          cowboy_req:stream_body(Body, nofin, Req1),
          Req1
      end,
      fastcgi_wait(Req2, Opts, true);

    {fast_cgi_stderr, _Id, Data} ->
      lager:error("fastcgi: ~p", [Data]),
      fastcgi_wait(Req0, Opts, StreamStarted);

    {fast_cgi_request_done, _Id} ->
      Req1 = cowboy_req:reply(500, #{
        <<"content-type">> => <<"text/plain">>
      }, <<"Error from upstream">>, Req0),
      {ok, Req1, Opts};

    {fast_cgi_done, _Id} ->
      cowboy_req:stream_body("", fin, Req0),
      {ok, Req0, Opts}
  after
    5000 ->
      Req1 = cowboy_req:reply(500, #{
        <<"content-type">> => <<"text/plain">>
      }, <<"Timeout from upstream">>, Req0),
      {ok, Req1, Opts}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Parses headers in binaries like <<"content-type: text/html>>" and saves
%% them into a map.
-spec parse_and_pack_headers([binary()]) -> map().
parse_and_pack_headers(Binaries) ->
  lists:foldl(
    fun(Header, AccHeaders) ->
      [Key, Value] = re:split(Header, <<":">>),
      KeyLower = list_to_binary(string:to_lower(binary_to_list(Key))),
      maps:put(KeyLower, Value, AccHeaders)
    end,
    #{<<"status">> => <<"200">>},
    Binaries
  ).

%% @doc Extracts headers and body from a binary string like:
%% <<"content-type: text/html\r\nthis is the body\r\n">>. Headers will not be
%% parsed (i.e: split again by ":")
-spec parse_headers_and_body(binary()) -> {[binary()], binary()}.
parse_headers_and_body(Binary) ->
  {RawHeaders, Body} = case re:split(Binary, <<"\r\n\r\n">>) of
    [AllHeadersBin] -> {AllHeadersBin, <<>>};
    [AllHeadersBin, SomeBodyBin] -> {AllHeadersBin, SomeBodyBin}
  end,
  SplitHeaders = re:split(RawHeaders, <<"\r\n">>),
  {SplitHeaders, Body}.

%% @doc Returns the HTTP status in a binary string like "404 Not Found".
-spec bin_to_http_status(binary()) -> pos_integer().
bin_to_http_status(Binary) ->
  {match, [Status, _]} = re:run(
    Binary, <<"(\\d\\d\\d)">>, [{capture, all, binary}]
  ),
  binary_to_integer(Status).

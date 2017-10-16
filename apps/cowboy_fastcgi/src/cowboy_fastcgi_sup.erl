%%% @doc Top level supervisor.
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
-module(cowboy_fastcgi_sup).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/cowboy_fastcgi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1]).
-export([start_link/1, fastcgi_run/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the supervisor.
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Wraps around erl_fastcgi:start_link/3 due to poolboy not doing an
%% erlang:apply to start the pool workers.
-spec start_link([term()]) -> {ok, pid()}.
start_link([Host, Port, ReconnectTime]) ->
  erl_fastcgi:start_link(Host, Port, ReconnectTime).

%% @doc Runs a FastCGI request.
-spec fastcgi_run(pos_integer(), proplists:proplist(), binary()) -> ok.
fastcgi_run(RequestId, Params, Body) ->
  Me = self(),
  poolboy:transaction(php_fpm, fun(Worker) ->
    gen_server:cast(Worker, {run, Me, RequestId, Params, Body})
  end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Supervisor callbacks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/supervisor.html#Module:init-1
-spec init(
  term()
) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/php-fpm/:script", cowboy_fastcgi_handler, []}
    ]}
  ]),

  Children = [
    poolboy:child_spec(
      php_fpm,
      [
        {name, {local, php_fpm}},
        {worker_module, ?MODULE},
        {size, ?CFG:php_fpm_workers()},
        {max_overflow, ?CFG:php_fpm_workers() * 2}
      ],
      [
        ?CFG:php_fpm_host(),
        ?CFG:php_fpm_port(),
        ?CFG:php_fpm_reconnect()
      ]
    ),
    {
      cowboy,
      {cowboy, start_clear, [
        http,
        [{port, ?CFG:http_port()}],
        #{
          env => #{dispatch => Dispatch}
        }
      ]},
      permanent,
      brutal_kill,
      worker,
      []
    }
  ],
  {ok, { {one_for_one, 0, 1}, Children} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

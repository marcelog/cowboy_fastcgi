%%% @doc Config helper.
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
-module(cowboy_fastcgi_cfg).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
  php_fpm_host/0,
  php_fpm_port/0,
  php_fpm_workers/0,
  php_fpm_reconnect/0,
  php_fpm_root/0
]).
-export([http_port/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns http listen port.
-spec http_port() -> pos_integer().
http_port() ->
  cfg_get(http_port, 8080).

%% @doc Returns time to wait between connection attempts to php-fpm.
-spec php_fpm_root() -> string().
php_fpm_root() ->
  cfg_get(php_fpm_root, "/tmp").

%% @doc Returns time to wait between connection attempts to php-fpm.
-spec php_fpm_reconnect() -> pos_integer().
php_fpm_reconnect() ->
  cfg_get(php_fpm_reconnect, 1000).

%% @doc Returns the total number of php-fpm workers.
-spec php_fpm_workers() -> pos_integer().
php_fpm_workers() ->
  cfg_get(php_fpm_workers, 10).

%% @doc Returns php-fpm host.
-spec php_fpm_host() -> string().
php_fpm_host() ->
  cfg_get(php_fpm_host, "127.0.0.1").

%% @doc Returns php-fpm port.
-spec php_fpm_port() -> pos_integer().
php_fpm_port() ->
  cfg_get(php_fpm_port, 9000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns an application config key or a default value.
-spec cfg_get(atom(), term()) -> term().
cfg_get(Key, Default) ->
  application:get_env(cowboy_fastcgi, Key, Default).

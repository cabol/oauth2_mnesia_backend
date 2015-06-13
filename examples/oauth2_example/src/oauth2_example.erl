-module(oauth2_example).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_phase/3, stop/0, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(oauth2_example).

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
  oauth2_example_sup:start_link().

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(start_oauth2_backend, _StartType, []) ->
  start_oauth2_backend(),
  ok;
start_phase(start_cowboy_listeners, _StartType, []) ->
  Routes     = cowboy_routes(),
  Dispatch   = cowboy_router:compile(Routes),
  ProtoOpts  = [{env, [{dispatch, Dispatch}]}],
  TransOpts  = application:get_env(oauth2_example, http_trans_opts, [{port, 9999}]),
  Cacceptors = application:get_env(oauth2_example, http_listener_count, 10),
  cowboy:start_http(http, Cacceptors, TransOpts, ProtoOpts),
  ok.

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(oauth2_example).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
start_oauth2_backend() ->
  Master = list_to_atom(
    application:get_env(oauth2_example, master_node, "master@127.0.0.1")),
  TablesConfig = application:get_env(oauth2, tables_config, undefined),
  case Master =:= node() of
    true ->
      lager:info("Init Master Node: ~p~n", [node()]),
      oauth2_mnesia_backend:start(TablesConfig, []);
    false ->
      lager:info("Init Slave Node: ~p~n", [node()]),
      net_adm:ping(Master),
      oauth2_mnesia_backend:start(TablesConfig, nodes())
  end.

%% @private
cowboy_routes() ->
  [
   {'_',
    [
     {"/auth", oauth2_example_auth, []},
     {"/resource", oauth2_example_resource, []}
    ]
   }
  ].

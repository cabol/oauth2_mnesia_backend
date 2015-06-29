%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc Mnesia backend for kivra oauth2.
%%% @see <a href="https://github.com/kivra/oauth2">OAuth2</a>
%%%-------------------------------------------------------------------
-module(oauth2_mnesia_backend).

-behavior(oauth2_backend).

%% API
-export([start/1,
         start/2,
         stop/0,
         get_user/1,
         add_user/2,
         delete_user/1,
         get_client/1,
         add_client/2,
         add_client/3,
         delete_client/1]).

%% Behavior API
-export([authenticate_user/2,
         authenticate_client/2,
         get_client_identity/2,
         associate_access_code/3,
         associate_refresh_token/3,
         associate_access_token/3,
         resolve_access_code/2,
         resolve_refresh_token/2,
         resolve_access_token/2,
         revoke_access_code/2,
         revoke_access_token/2,
         revoke_refresh_token/2,
         get_redirection_uri/2,
         verify_redirection_uri/3,
         verify_client_scope/3,
         verify_resowner_scope/3,
         verify_scope/3]).

%%%===================================================================
%%% Types and Macros
%%%===================================================================

%% Tables Config
-type auth_tab()   :: access_token | refresh_token | user | client.
-type copies()     :: ram_copies | disc_copies | disc_only_copies.
-type tab_config() :: [{auth_tab(), copies()}].

%% Tables
-define(ACCESS_TOKEN_TABLE, access_token).
-define(REFRESH_TOKEN_TABLE, refresh_token).
-define(USER_TABLE, user).
-define(CLIENT_TABLE, client).

%% Table list
-define(TABLES, [?ACCESS_TOKEN_TABLE,
                 ?REFRESH_TOKEN_TABLE,
                 ?USER_TABLE,
                 ?CLIENT_TABLE]).

%% Timeout for mnesia:wait_for_tables
-define(WAIT_FOR_TABLES, 5000).

%% Access token spec
-record(access_token, {token        :: binary(),
                       context = [] :: proplists:proplist()}).

%% Refresh token spec
-record(refresh_token, {token        :: binary(),
                        context = [] :: proplists:proplist()}).

%% User spec
-record(user, {username :: binary(),
               password :: binary()}).
-type user() :: #user{}.

%% Client spec
-record(client, {client_id     :: binary(),
                 client_secret :: binary(),
                 redirect_uri  :: binary()}).
-type client() :: #client{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start([node()]) -> ok.
start(Nodes) ->
  start(undefined, Nodes).

-spec start(tab_config(), [node()]) -> ok.
start(TablesConfig, Nodes) ->
  mnesia:stop(),
  mnesia:create_schema(Nodes),
  mnesia:start(),
  dynamic_db_init(TablesConfig, Nodes),
  ok.

-spec stop() -> ok.
stop() ->
  ok.

-spec get_user(binary()) -> user().
get_user(Username) ->
  get(?USER_TABLE, Username).

-spec add_user(binary(), binary()) -> ok.
add_user(Username, Password) ->
  put(?USER_TABLE, Username, #user{username = Username, password = Password}).

-spec delete_user(binary()) -> ok.
delete_user(Username) ->
  delete(?USER_TABLE, Username).

-spec get_client(binary()) -> client().
get_client(ClientId) ->
  get(?CLIENT_TABLE, ClientId).

-spec add_client(binary(), binary(), binary()) -> ok.
add_client(Id, Secret, RedirectUri) ->
  put(?CLIENT_TABLE, Id, #client{client_id = Id,
                                 client_secret = Secret,
                                 redirect_uri = RedirectUri}).

-spec add_client(binary(), binary()) -> ok.
add_client(Id, Secret) ->
  add_client(Id, Secret, undefined).

-spec delete_client(binary()) -> ok.
delete_client(Id) ->
  delete(?CLIENT_TABLE, Id).

%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================

%% @hidden
authenticate_user({Username, Password}, _) ->
  case get(?USER_TABLE, Username) of
    {ok, #user{password = Password}} ->
      {ok, {<<"user">>, Username}};
    {ok, #user{password = _WrongPassword}} ->
      {error, badpass};
    Error = {error, notfound} ->
      Error
  end.

%% @hidden
authenticate_client({ClientId, ClientSecret}, _) ->
  case get(?CLIENT_TABLE, ClientId) of
    {ok, #client{client_secret = ClientSecret}} ->
      {ok, {<<"client">>, ClientId}};
    {ok, #client{client_secret = _WrongSecret}} ->
      {error, badsecret};
    _ ->
      {error, notfound}
  end.

%% @hidden
get_client_identity(ClientId, AppCtx) ->
  case get(?CLIENT_TABLE, ClientId) of
    {ok, Client} ->
      {ok, {AppCtx, Client}};
    _ ->
      {error, notfound}
  end.

%% @hidden
associate_access_code(AccessCode, GrantCtx, AppCtx) ->
  associate_access_token(AccessCode, GrantCtx, AppCtx).

%% @hidden
associate_access_token(AccessToken, GrantCtx, AppCtx) ->
  AccessTokenRec = #access_token{token = AccessToken, context = GrantCtx},
  put(?ACCESS_TOKEN_TABLE, AccessToken, AccessTokenRec),
  {ok, AppCtx}.

%% @hidden
associate_refresh_token(RefreshToken, GrantCtx, AppCtx) ->
  RefreshTokenRec = #access_token{token = RefreshToken, context = GrantCtx},
  put(?REFRESH_TOKEN_TABLE, RefreshToken, RefreshTokenRec),
  {ok, AppCtx}.

%% @hidden
resolve_access_code(AccessCode, AppCtx) ->
  resolve_access_token(AccessCode, AppCtx).

%% @hidden
resolve_refresh_token(RefreshToken, AppCtx) ->
  resolve_access_token(RefreshToken, AppCtx).

%% @hidden
resolve_access_token(AccessToken, AppCtx) ->
  %% The case trickery is just here to make sure that
  %% we don't propagate errors that cannot be legally
  %% returned from this function according to the spec.
  case get(?ACCESS_TOKEN_TABLE, AccessToken) of
    {ok, #access_token{context = Value}} ->
      {ok, {AppCtx, Value}};
    Error = {error, notfound} ->
      Error
  end.

%% @hidden
revoke_access_code(AccessCode, AppCtx) ->
  revoke_access_token(AccessCode, AppCtx).

%% @hidden
revoke_access_token(AccessToken, AppCtx) ->
  delete(?ACCESS_TOKEN_TABLE, AccessToken),
  {ok, AppCtx}.

%% @hidden
revoke_refresh_token(_RefreshToken, AppCtx) ->
  {ok, AppCtx}.

%% @hidden
get_redirection_uri(ClientId, AppCtx) ->
  case get(?CLIENT_TABLE, ClientId) of
    {ok, #client{redirect_uri = RedirectUri}} ->
      {ok, {AppCtx, RedirectUri}};
    Error = {error, notfound} ->
      Error
  end.

%% @hidden
verify_redirection_uri(ClientId, ClientUri, AppCtx) ->
  case get(?CLIENT_TABLE, ClientId) of
    {ok, #client{redirect_uri = RedirUri}} when ClientUri =:= RedirUri ->
      {ok, AppCtx};
    _Error ->
      {error, mismatch}
  end.

%% @hidden
verify_client_scope(_Client, Scope, AppCtx) ->
  {ok, {AppCtx, Scope}}.

%% @hidden
verify_resowner_scope(_ResOwner, Scope, AppCtx) ->
  {ok, {AppCtx, Scope}}.

%% @hidden
verify_scope(Scope, Scope, AppCtx) ->
  {ok, {AppCtx, Scope}};
verify_scope(_, _, _) ->
  {error, invalid_scope}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
dynamic_db_init(undefined, []) ->
  DefaultConfig = lists:zip(
    ?TABLES, [ram_copies, ram_copies, disc_copies, disc_copies]),
  dynamic_db_init(DefaultConfig, []);
dynamic_db_init(TablesConfig, []) ->
  create_tables(TablesConfig);
dynamic_db_init(TablesConfig, Nodes) ->
  add_extra_nodes(TablesConfig, Nodes).

%% @private
add_extra_nodes(TablesConfig, [Node | T]) ->
  case mnesia:change_config(extra_db_nodes, [Node]) of
    {ok, [Node]} ->
      %% replaces local schema with remote
      mnesia:change_table_copy_type(schema, node(), disc_copies),
      %% add table copies
      [mnesia:add_table_copy(Tab, node(), Cp) || {Tab, Cp} <- TablesConfig],
      %% synchronization
      Tables = mnesia:system_info(tables),
      mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
    _ ->
      add_extra_nodes(TablesConfig, T)
  end.

%% @private
create_tables([]) ->
  ok;
create_tables([{?ACCESS_TOKEN_TABLE, Copies} | T]) ->
  mnesia:create_table(
    ?ACCESS_TOKEN_TABLE,
    [{Copies, [node()]},
     {attributes, record_info(fields, ?ACCESS_TOKEN_TABLE)}]),
  create_tables(T);
create_tables([{?REFRESH_TOKEN_TABLE, Copies} | T]) ->
  mnesia:create_table(
    ?REFRESH_TOKEN_TABLE,
    [{Copies, [node()]},
     {attributes, record_info(fields, ?REFRESH_TOKEN_TABLE)}]),
  create_tables(T);
create_tables([{?USER_TABLE, Copies} | T]) ->
  mnesia:create_table(
    ?USER_TABLE,
    [{Copies, [node()]},
     {attributes, record_info(fields, ?USER_TABLE)}]),
  create_tables(T);
create_tables([{?CLIENT_TABLE, Copies} | T]) ->
  mnesia:create_table(
    ?CLIENT_TABLE,
    [{Copies, [node()]},
     {attributes, record_info(fields, ?CLIENT_TABLE)}]),
  create_tables(T).

%% @private
get(Table, Key) ->
  case mnesia:dirty_read(Table, Key) of
    []      -> {error, notfound};
    [Value] -> {ok, Value}
  end.

%% @private
put(Table, _Key, Value) ->
  mnesia:dirty_write(Table, Value).

%% @private
delete(Table, Key) ->
  mnesia:dirty_delete(Table, Key).

-module(oauth2_example_resource).

%% Cowboy behavior
-export([init/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2]).

%% Handlers
-export([process_get/2, process_put/2]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @hidden
init(Req, _Opts) ->
  {cowboy_rest, Req, #{}}.

%% @hidden
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

%% @hidden
is_authorized(Req, State) ->
  oauth2_example_authorizer:is_authorized(Req, State).

%% @hidden
content_types_accepted(Req, State) ->
  ContentTypes = [{{<<"application">>, <<"json">>, '*'}, process_put}],
  {ContentTypes, Req, State}.

%% @hidden
content_types_provided(Req, State) ->
  {[{<<"application/json">>, process_get}], Req, State}.

%% @hidden
process_put(Req, State) ->
  {true, Req, State}.

%% @hidden
process_get(Req, State) ->
  {<<>>, Req, State}.

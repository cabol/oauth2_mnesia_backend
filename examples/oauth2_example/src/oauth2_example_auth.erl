-module(oauth2_example_auth).

-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2]).

-export([handle_post/2, handle_get/2]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @hidden
init(Req, _Opts) ->
  %% Compile the DTL template used for the authentication
  %% form in the implicit grant flow.
  %%{ok, auth_form} = erlydtl:compile(
  %%  filename:join(["priv", "static", "auth_form.dtl"]), auth_form),
  {cowboy_rest, Req, #{}}.

%% @hidden
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

%% @hidden
content_types_provided(Req, State) ->
  ContentTypes =
    [{<<"application/json">>, handle_get},
     {<<"test/html">>, handle_get}],
  {ContentTypes, Req, State}.

content_types_accepted(Req, State) ->
  ContentTypes =
    [{{<<"application">>, <<"json">>, '*'}, handle_post},
     {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_post}],
  {ContentTypes, Req, State}.

%% @hidden
handle_post(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  try
    Params = decode_form(Body),
    case oauth2_example_util:keyfind(<<"grant_type">>, Params) of
      <<"password">> ->
        process_password_grant(Req1, Params, State);
      <<"client_credentials">> ->
        process_client_credentials_grant(Req1, Params, State);
      <<"token">> ->
        ResBody = oauth2_example_util:enc_json(#{error => 'NOT_SUPPORTED'}),
        Req2 = cowboy_req:set_resp_body(ResBody, Req),
        {false, Req2, State};
      _ ->
        ResBody = oauth2_example_util:enc_json(#{error => 'UNKNOWN_GRANT_TYPE'}),
        Req2 = cowboy_req:set_resp_body(ResBody, Req),
        {false, Req2, State}
    end
  catch
    _:Ex ->
      lager:warning("~p -> Stack Trace: ~p~n", [Ex, erlang:get_stacktrace()]),
      oauth2_example_util:handle_exception(Ex, Req, State)
  end.

%% @hidden
handle_get(Req, State) ->
  {<<"{}">>, Req, State}.

%%%===================================================================
%%% Grant type handlers
%%%===================================================================

%% @private
process_password_grant(Req, Params, State) ->
  Username = oauth2_example_util:keyfind(<<"username">>, Params),
  Password = oauth2_example_util:keyfind(<<"password">>, Params),
  Scope = oauth2_example_util:keyfind(<<"scope">>, Params, <<"">>),
  case {Username, Password} of
    _ when Username =:= undefined; Password =:= undefined ->
      throw(bad_request);
    _ ->
      Auth = oauth2:authorize_password({Username, Password}, Scope, []),
      issue_token(Auth, Req, State)
  end.

%% @private
process_client_credentials_grant(Req, Params, State) ->
  case catch(cowboy_req:parse_header(<<"authorization">>, Req)) of
    {basic, Id, Secret} ->
      Scope = oauth2_example_util:keyfind(<<"scope">>, Params),
      Auth = oauth2:authorize_client_credentials({Id, Secret}, Scope, []),
      issue_token(Auth, Req, State);
    _ ->
      throw(bad_request)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
issue_token({ok, {_, Auth}}, Req, State) ->
  emit_response(oauth2:issue_token(Auth, []), Req, State);
issue_token(Error, Req, State) ->
  emit_response(Error, Req, State).

%% @private
emit_response(AuthResult, Req, State) ->
  case AuthResult of
    {error, Reason} ->
      Body = oauth2_example_util:enc_json(#{error => Reason}),
      Req1 = cowboy_req:set_resp_body(Body, Req),
      {false, Req1, State};
    {ok, {_, Response}} ->
      Body = oauth2_example_util:enc_json(maps:from_list(
        oauth2_response:to_proplist(Response))),
      Req1 = cowboy_req:set_resp_body(Body, Req),
      {true, Req1, State}
  end.

%% @private
decode_form(Form) ->
  try cow_qs:parse_qs(Form)
  catch
    _:_ -> throw(bad_request)
  end.

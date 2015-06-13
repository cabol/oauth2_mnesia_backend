-module(oauth2_example_authorizer).

%% API
-export([is_authorized/2]).

-define(AUTH_HEADER, <<"Bearer">>).

%%%===================================================================
%%% API
%%%===================================================================

is_authorized(Req, State) ->
  try
    case get_access_token(Req) of
      {ok, Token} ->
        case oauth2:verify_access_token(Token, []) of
          {ok, _Identity} ->
            {true, Req, State};
          {error, access_denied} ->
            {{false, ?AUTH_HEADER}, Req, State}
        end;
      {error, _} ->
        {{false, ?AUTH_HEADER}, Req, State}
    end
  catch
    E:Ex ->
      ErrorMsg = "~p:~p -> Unexpected error.~n\tStack: ~p~n",
      lager:error(ErrorMsg, [E, Ex, erlang:get_stacktrace()]),
      {{false, ?AUTH_HEADER}, Req, State}
  end.

get_access_token(Req) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {bearer, Token} ->
      {ok, Token};
    _ ->
      #{access_token := AccessToken} = cowboy_req:match_qs(
        [{access_token, nonempty, undefined}], Req),
      case AccessToken of
        undefined -> {error, missing};
        _         -> {ok, AccessToken}
      end
  end.

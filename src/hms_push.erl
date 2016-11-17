-module(hms_push).

%%API
-export([get_access_token_info/0, get_access_token_info/2,
         single_send/1, single_send/2, single_send/3, single_send/4,
         ps_single_send/1, ps_single_send/3, ps_single_send/4, ps_single_send/5,
         batch_send/1, batch_send/2, batch_send/3, batch_send/4,
         ps_batch_send/1, ps_batch_send/3, ps_batch_send/4, ps_batch_send/5
        ]).

-include("hms_push.hrl").
-include_lib("eutil/include/eutil.hrl").

%%return code
-define(ACCESS_TOKEN_EXPIRE, 6).

-define(PUSH_TYPE_TOKENS, 1).
-define(PUSH_TYPE_ALL, 2).
-define(PUSH_TYPE_TAGS, 3).

get_access_token_info() ->
    {ok, AppId} = application:get_env(hms_push, app_id),
    {ok, AppSecret} = application:get_env(hms_push, app_secret),
    get_access_token_info(AppId, AppSecret).

get_access_token_info(AppId, AppSecret) ->
    Datas = [{grant_type, "client_credentials"}, {client_id, AppId},
             {client_secret, AppSecret}],
    Method = post,
    Payload = eutil:urlencode(Datas),
    Options = [{pool, default}],
    {ok, _Code, _Headers, ClientRef} = hackney:request(Method, ?HMS_TOKEN_URL, ?URLENCEDED_HEADS,
                                                       Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    jiffy:decode(ResultBin, [return_maps]).

get_access_token() ->
    TokenInfo = get_access_token_info(),
    maps:get(<<"access_token">>, TokenInfo).

get_access_token(AppId, AppSecret) ->
    TokenInfo = get_access_token_info(AppId, AppSecret),
    maps:get(<<"access_token">>, TokenInfo).

single_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = case maps:get(<<"access_token">>, PayloadMaps, undefined) of
                     undefined ->
                         maps:merge(?HMS_SINGLE_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps);
                     AccessToken ->
                         PayloadMaps(?HMS_SINGLE_ARGS, PayloadMaps)
                 end,
    send(NewPayload).

single_send(DeviceToken, Message) ->
    AccessToken = get_access_token(),
    single_send(AccessToken, DeviceToken, Message).

single_send(AccessToken, DeviceToken, Message) ->
    Payload = ?HMS_SINGLE_ARGS#{<<"access_token">> => AccessToken,
                                <<"deviceToken">> => list_to_binary(DeviceToken),
                                <<"message">> => Message},
    send(Payload).

single_send(AppId, AppSecret, DeviceToken, Message) ->
    AccessToken = get_access_token(AppId, AppSecret),
    single_send(AccessToken, DeviceToken, Message).


ps_single_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HMS_PS_SINGLE_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

ps_single_send(DeviceToken, Title, Content) ->
    AccessToken = get_access_token(),
    ps_single_send(AccessToken, DeviceToken, Title, Content).

ps_single_send(AccessToken, DeviceToken, Title, Content) ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HMS_PS_SINGLE_ARGS#{<<"access_token">> => AccessToken,
                                      <<"deviceToken">> => DeviceToken,
                                      <<"android">> => AndroidMsg},
    send(NewPayload).

ps_single_send(AppId, AppSecret, DeviceToken, Title, Content) ->
    AccessToken = get_access_token(AppId, AppSecret),
    ps_single_send(AccessToken, DeviceToken, Title, Content).


batch_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HMS_BATCH_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

batch_send(DeviceTokens, Message) ->
    AccessToken = get_access_token(),
    batch_send(AccessToken, DeviceTokens, Message).

batch_send(AccessToken, DeviceTokens, Message) ->
    NewList = lists:flatten(io_lib:format("~p", [DeviceTokens])),
    Payload = ?HMS_BATCH_ARGS#{<<"access_token">> => AccessToken,
                               <<"deviceTokenList">> => NewList,
                               <<"message">> => Message},
    send(Payload).

batch_send(AppId, AppSecret, DeviceTokens, Message) ->
    AccessToken = get_access_token(AppId, AppSecret),
    batch_send(AccessToken, DeviceTokens, Message).


ps_batch_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HMS_PS_BATCH_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

ps_batch_send(DeviceTokens, Title, Content) ->
    AccessToken = get_access_token(),
    ps_batch_send(AccessToken, DeviceTokens, Title, Content).

ps_batch_send(AccessToken, DeviceTokens, Title, Content) ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HMS_PS_BATCH_ARGS#{<<"access_token">> => AccessToken,
                                      <<"deviceTokenList">> => DeviceTokens,
                                      <<"android">> => AndroidMsg},
    send(NewPayload).

ps_batch_send(AppId, AppSecret, DeviceTokens, Title, Content) ->
    AccessToken = get_access_token(AppId, AppSecret),
    ps_batch_send(AccessToken, DeviceTokens, Title, Content).




do_send(PayloadMaps) ->
    Method = post,
    Payload = eutil:urlencode(PayloadMaps),
    Options = [{pool, default}],
    {ok, _Code, _Headers, ClientRef} = hackney:request(Method, ?HMS_API_URL, ?URLENCEDED_HEADS,
                                                       Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    jiffy:decode(ResultBin, [return_maps]).

send(PayloadMaps) ->
    ResultOri = do_send(PayloadMaps),
    Result = case erlang:is_map(ResultOri) of
                 true -> ResultOri;
                 false -> jiffy:decode(ResultOri, [return_maps])
             end,
    Code = case maps:get(<<"resultcode">>, Result, undefined) of
               undefined -> maps:get(<<"result_code">>, Result);
               Other -> Other
           end,
    case Code of
        ?SUCCESS_0 ->
            {ok, Code};
        ?ACCESS_TOKEN_EXPIRE ->
            {access_token_expire, Code};
        _ ->
            ?ERROR_MSG("huawei_push error, PayloadMaps: ~p, Result: ~p", [PayloadMaps, Result]),
            {error, Code}
    end.


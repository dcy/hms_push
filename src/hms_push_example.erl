-module(hms_push_example).
-compile(export_all).

-define(DEVICE_TOKEN, <<"0867065025020236300000198100CN01">>).
-define(DEVICE_TOKENS, ["0867065025020236300000198100CN01", "1867065025020236300000198100CN02", "12323"]).


single_send(Msg) ->
    hms_push:single_send(?DEVICE_TOKEN, Msg).

ps_single_send(Title, Content) ->
    hms_push:ps_single_send(?DEVICE_TOKEN, Title, Content).

general_notification(Title, Content) ->
    TokenInfo = hms_push:get_access_token_info(),
    AccessToken = maps:get(<<"access_token">>, TokenInfo),
    hms_push:general_notification(AccessToken, ?DEVICE_TOKEN, Title, Content).

batch_send(Msg) ->
    hms_push:batch_send(?DEVICE_TOKENS, Msg).

ps_batch_send(Title, Content) ->
    hms_push:ps_batch_send(?DEVICE_TOKENS, Title, Content).

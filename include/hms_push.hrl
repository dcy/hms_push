-ifndef(__HMS_PUSH_HRL__).
-define(__HMS_PUSH_HRL__, 0).

-define(HMS_TOKEN_URL, <<"https://login.vmall.com/oauth2/token">>).
-define(HMS_API_URL, <<"https://api.vmall.com/rest.php">>).

-define(HMS_SINGLE_ARGS, #{<<"nsp_svc">> => <<"openpush.message.single_send">>, 
                           <<"nsp_ts">> => erlang:system_time(seconds),
                           <<"priority">> => 1, <<"cacheMode">> => 0, <<"msgType">> => -1}).

-define(HMS_PS_SINGLE_ARGS, #{<<"nsp_svc">> => <<"openpush.message.psSingleSend">>,
                              <<"nsp_ts">> => erlang:system_time(seconds),
                              <<"cacheMode">> => 0, <<"msgType">> => -1}).

-define(HMS_BATCH_ARGS, #{<<"nsp_svc">> => <<"openpush.message.batch_send">>,
                          <<"nsp_ts">> => erlang:system_time(seconds),
                          <<"cacheMode">> => 0, <<"msgType">> => -1}).

-define(HMS_PS_BATCH_ARGS, #{<<"nsp_svc">> => <<"openpush.message.psBatchSend">>,
                             <<"nsp_ts">> => erlang:system_time(seconds),
                             <<"cacheMode">> => 0, <<"msgType">> => -1}).

-define(HMS_QUERY_ARGS, #{<<"nsp_svc">> => <<"openpush.openapi.query_msg_result">>,
                          <<"nsp_ts">> => erlang:system_time(seconds)}).


-endif.

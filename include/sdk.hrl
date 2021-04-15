%
% Nitro/js forms
%
-ifdef(ELEMENT_BASE).

-record(email_sdk,      { ?ELEMENT_BASE(email_sdk) }).
-record(cipher_sdk,     { ?ELEMENT_BASE(cipher_sdk) }).
-record(telegram_sdk,   { ?ELEMENT_BASE(telegram_sdk) }).

-endif.

-define(CIPHER_JNLP, application:get_env(avz,cipher_jnlp,"https://caas.infotech.gov.ua/pcs/prod-internal/pcs-infotech.jnlp")).
-define(CIPHER_URL, io_lib:format("~s:~p", [
    application:get_env(avz,cipher_url,"https://local.cipher.kiev.ua"),
    application:get_env(avz,cipher_port,9090) ])).

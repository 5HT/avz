-module(telegram_sdk).
-include_lib("nitro/include/nitro.hrl").
-include_lib("avz/include/proto.hrl").
-include("sdk.hrl").
-export([render_element/1]).

env(N,D) -> nitro:to_list(application:get_env(avz,N,D)).

render_element(#telegram_sdk{}=R) when R#telegram_sdk.show_if=:= false -> [<<>>];
render_element(#telegram_sdk{id=Id}) ->
    Cid = case Id of [] -> nitro:temp_id(); I -> I end,
    Fid = "ontauth",

    IO = iolist_to_binary([
        "window.",Fid,"=",
        "function(user){",
            nitro:f("ws.send(~s);", [?AVZ(tlg,Cid,login,"bin(JSON.stringify(user))")]),
        "};"
    ]),

    nitro:wire(IO),

    nitro:wire(iolist_to_binary(["(function(){",
        "let  s= qn('script');",
        "s.async = true;",
        "s.src ='", env(tl, "https://telegram.org/js/telegram-widget.js?14"), "';",
        "s.setAttribute('data-telegram-login','", env(tl_bot, dxt_bot), "');",
        "s.setAttribute('data-size','",           env(tl_btn_size, medium), "');",
        "s.setAttribute('data-radius','",         env(tl_btn_radius, 10), "');",
        "s.setAttribute('data-request-access','", env(tl_req_access, write), "');",
        "s.setAttribute('data-userpic',",         env(tl_userpic, true),");",
        "s.setAttribute('data-onauth', '",Fid,"(user)');",
        "document.body.appendChild(s);",
    "})();" ])),
    nitro:render(#panel{id=Cid}).

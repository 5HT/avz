-module(telegram_sdk).
-include_lib("nitro/include/nitro.hrl").
-include_lib("avz/include/proto.hrl").
-include("sdk.hrl").
-export([render_element/1]).

env(N,D) -> nitro:to_list(application:get_env(avz,N,D)).

render_element(#telegram_sdk{}=R) when R#telegram_sdk.show_if=:= false -> [<<>>];
render_element(#telegram_sdk{id=Id}) ->
    Cid = case Id of [] -> nitro:temp_id(); I -> I end,
    Soid = nitro:temp_id(), Suid = nitro:temp_id(),
    Fid = "ontauth",

    IO = iolist_to_binary([
        "window.",Fid,"=",
        "function(user){",
            nitro:f("ws.send(~s);", [?AVZ(tlg,Cid,login,"bin(JSON.stringify(user))")]),
        "};"
    ]),
    nitro:wire(IO),
    Logout = nitro:f("ws.send(~s);", [?AVZ(tlg, Cid, logout)]),

    OnError = iolist_to_binary(["qi('",Cid,"').innerHTML=event.detail.data();"]),
    OnLogin = iolist_to_binary([
        "qi('",Suid,"').style.setProperty('display', 'none');",
        "qi('",Soid,"').style.setProperty('display', 'block');"
    ]),
    OnLogout = iolist_to_binary([
        "qi('",Suid,"').style.setProperty('display', 'block');",
        "qi('",Soid,"').style.setProperty('display', 'none');"
    ]),

    nitro:wire(#bind{target=Cid, type=login,  postback=OnLogin}),
    nitro:wire(#bind{target=Cid, type=logout, postback=OnLogout}),
    nitro:wire(#bind{target=Cid, type=error,  postback=OnError}),
    nitro:wire(#bind{target=Soid, type=click, postback=Logout}),

    nitro:wire(iolist_to_binary(["(function(){",
        "let  s= qn('script'), w = qn('div');",
        "s.async = true;",
        "s.src ='", env(tl, "https://telegram.org/js/telegram-widget.js?14"), "';",
        "s.setAttribute('data-telegram-login','", env(tl_bot, dxt_bot), "');",
        "s.setAttribute('data-size','",           env(tl_btn_size, medium), "');",
        "s.setAttribute('data-radius','",         env(tl_btn_radius, 10), "');",
        "s.setAttribute('data-request-access','", env(tl_req_access, write), "');",
        "s.setAttribute('data-userpic',",         env(tl_userpic, true),");",
        "s.setAttribute('data-onauth', '",Fid,"(user)');",
        "w.setAttribute('id', '",Suid,"');",
        "w.appendChild(s);",
        "qi('",Cid,"').appendChild(w);",
    "})();" ])),

    nitro:wire(#jq{target={ps, Soid, "style"}, method=["setProperty"], args=["'display','none'"]}),

    nitro:render(#panel{id=Cid, body=[
        #button{id=Soid, class=btn, body= <<"logout"/utf8>>}
    ]}).

-module(email_sdk).
-include_lib("nitro/include/nitro.hrl").
-include("proto.hrl").
-include("sdk.hrl").
-export([render_element/1]).

render_element(#email_sdk{}=R) when R#email_sdk.show_if=:= false -> [<<>>];
render_element(#email_sdk{id=Id,class=Class,title=T}) ->
    Cid = case Id of [] -> nitro:temp_id();I -> I end,
    Mid = nitro:temp_id(), Psid = nitro:temp_id(), Suid = nitro:temp_id(), Stid = nitro:temp_id(), Soid = nitro:temp_id(),
    Rgid = nitro:temp_id(),Ttid = nitro:temp_id(),

    Source = [{email,Mid},{pass,Psid}],
    S1 = lists:join(",", [iolist_to_binary(["tuple(atom('",atom_to_list(A),"'),", "querySource('",S,"'))"]) || {A,S} <- Source]),
    S2 = iolist_to_binary(["list(", S1,")"]),

    Init   = nitro:f("ws.send(~s);", [?AVZ(eml, Cid, init)]),
    Submit = nitro:f("ws.send(~s);", [?AVZ(eml, Cid, login, S2)]),
    Logout = nitro:f("ws.send(~s);", [?AVZ(eml, Cid, logout)]),
    Regstr = nitro:f("ws.send(~s);", [?AVZ(eml, Cid, register, S2)]),

    Title = #h4{id=Ttid, class=title, body=T},
    Status = #panel{id=Stid, class=status},
    Form = [
        #panel{class=box, body=[
            #label{class=label, for=user, body= <<"електронна пошта"/utf8>>},
            #input{id=Mid, class=field}
        ]},

        #panel{class=box, body=[
            #label{class=label, for=password, body= <<"пароль"/utf8>>},
            #password{id=Psid, class=field}
        ]},

        #panel{class=box, body=[
            #button{id=Suid, class=btn, body= <<"вхід"/utf8>>},
            #button{id=Soid, class=btn, body= <<"вихід"/utf8>>},
            #button{id=Rgid, class=btn, body= <<"реєстрація"/utf8>>}
        ]}],

    OnError = iolist_to_binary(["qi('",Stid,"').innerHTML=event.detail.data();"]),
    OnLogin = iolist_to_binary([
        "qi('",Stid,"').innerHTML=event.detail.data();",
        "qi('",Ttid,"').style.setProperty('display', 'none');",
        "qi('",Suid,"').style.setProperty('display', 'none');",
        "qi('",Rgid,"').style.setProperty('display', 'none');",
        "qi('",Soid,"').style.setProperty('display', 'block');",
        "qi('",Mid,"').parentNode.style.setProperty('display','none');",
        "qi('",Psid,"').parentNode.style.setProperty('display','none');"
    ]),
    OnLogout = iolist_to_binary([
        "qi('",Stid,"').innerHTML='';",
        "qi('",Mid,"').value='';",
        "qi('",Psid,"').value='';",
        "qi('",Ttid,"').style.setProperty('display', 'block');",
        "qi('",Suid,"').style.setProperty('display', 'block');",
        "qi('",Rgid,"').style.setProperty('display', 'block');",
        "qi('",Soid,"').style.setProperty('display', 'none');",
        "qi('",Mid,"').parentNode.style.setProperty('display','block');",
        "qi('",Psid,"').parentNode.style.setProperty('display','block');"
    ]),

    nitro:wire(nitro:f("(function(){~s})();", [Init])),
    nitro:wire(#jq{target={ps, Soid, "style"}, method=["setProperty"], args=["'display','none'"]}),
    nitro:wire(#bind{target=Suid, type=click, postback=Submit,source=Source}),
    nitro:wire(#bind{target=Soid, type=click, postback=Logout}),
    nitro:wire(#bind{target=Rgid, type=click, postback=Regstr, source=Source}),
    nitro:wire(#bind{target=Cid, type=login,  postback=OnLogin}),
    nitro:wire(#bind{target=Cid, type=logout, postback=OnLogout}),
    nitro:wire(#bind{target=Cid, type=error,  postback=OnError}),

    nitro:render(#panel{id=Cid,class=Class, body=[Title, Status, Form]}).

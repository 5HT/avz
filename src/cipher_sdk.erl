-module(cipher_sdk).
-description("Login form for Cipher crypto service.").
-include_lib("nitro/include/nitro.hrl").
-include("proto.hrl").
-include("sdk.hrl").
-export([render_element/1]).

render_element(#cipher_sdk{}=Rec) when Rec#cipher_sdk.show_if =:= false -> [<<>>];
render_element(#cipher_sdk{id=Id,class=Class,title=T}) ->
    Cid = case Id of [] -> nitro:temp_id(); I -> I end,
    Bid=nitro:temp_id(),Suid=nitro:temp_id(),Soid=nitro:temp_id(),Rid=nitro:temp_id(),
    Stid=nitro:temp_id(),Ssid=nitro:temp_id(),Phid=nitro:temp_id(),Psid=nitro:temp_id(),
    Tpid=nitro:temp_id(),Caid=nitro:temp_id(),
    
    Source=[{ca,Caid},{type,Tpid},{key,Phid},{pass,Psid}],
    S1 = lists:join(",", [iolist_to_binary(["tuple(atom('",atom_to_list(A),"'),", "querySource('",S,"'))"]) || {A,S} <- Source]),
    S2 = iolist_to_binary(["list(", S1,")"]),

    Title = case T of [] -> <<>>; _ -> #h5{class=title, body=T} end,

    Form = [
        #panel{id=Stid,  class=status},
        #panel{id=Ssid, class=session},

        #panel{class=box, body=[
            #label{for=Caid, class=label, body= <<"КНЕДП/АЦСК"/utf8>>},
            #select{id=Caid, class=field, body=[]}
        ]},
        #panel{class=box, body=[
            #label{for=Tpid, class=label, body= <<"Тип ключа"/utf8>>},
            #select{id=Tpid, class=field, body=[
                #option{label= <<"Файл"/utf8>>, value=file},
                #option{label= <<"PKCS#11 пристрій - активний режим"/utf8>>, value=pkcs11active},
                #option{label= <<"PKCS#11 пристрій - пасивний режим"/utf8>>, value=pkcs11passive},
                #option{label= <<"MobileID"/utf8>>, value=mobileid}
            ]}
        ]},
        #panel{class=box, body=[
            #label{for=Phid, class=label, body= <<"Шлях до контейнеру"/utf8>>}, %<<"Вибрати токен"/utf8>>
            #panel{class=field, body=[
                #textbox{id=Phid, class=field, readonly=true},
                #link{id=Bid, class=btn, body= <<"Вибрати файл"/utf8>>, href="#"}
            ]}
        ]},
        #panel{class=box, body=[
            #label{for=Psid, class=label, body= <<"Пароль"/utf8>>},
            #password{id=Psid, class=field}
        ]},

        #panel{class=box, body=[
            #link{id=Suid,  class=btn, body= <<"Вхід"/utf8>>},
            #link{id=Soid,  class=btn, body= <<"Вихід"/utf8>>},
            #link{id=Rid,   class=btn, body= <<"Оновити"/utf8>>}
        ]}
    ],

    Init    = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,init)]),
    ReqKey  = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,req_key)]),
    Submit  = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,submit, S2)]),
    Logout  = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,logout)]),

    OnError  = iolist_to_binary([
        "qi('",Stid,"').innerHTML='<div>",
        <<"Криптографічний сервіс"/utf8>>, " <b>",?CIPHER_URL,"</b>", <<"не доступний."/utf8>>, ",</div>",
        "<div>",<<"Запустіть Сipher сервіс локально та перевірте CORS налаштування браузера."/utf8>>,"</div>",
        "<div>",<<"Завантажити java агент можна за посиланням:"/utf8>>,
        " <a href=",?CIPHER_JNLP,">pcs-infotech.jnlp</a></div>';"
    ]),
    OnSession = iolist_to_binary(["qi('",Ssid,"').innerHTML=event.detail.html();"]),
    OnStatus  = iolist_to_binary(["qi('",Stid,"').innerHTML=event.detail.html();"]),
    OnPath    = iolist_to_binary(["qi('",Phid,"').value=event.detail.html();"]),
    OnCa      = iolist_to_binary(["let s=qi('",Caid,"'),j=event.detail.ca.apply(),j1=JSON.parse(j);",
        "j1.forEach(({label:l, value:v}) => {let o=qn('option');o.value=v;o.label=l;s.add(o);});"]),

    OnLogin   = iolist_to_binary([
        "qi('",Suid,"').style.setProperty('display', 'none');",
        "qi('",Soid,"').style.setProperty('display', 'block');"
    ]),
    
    nitro:wire(nitro:f("(function(){~s})();", [Init])),
    nitro:wire(#jq{target={ps, Soid, "style"}, method=["setProperty"], args=["'display','none'"]}),
    nitro:wire(#jq{target={ps, Rid,  "style"}, method=["setProperty"], args=["'display','none'"]}),
    nitro:wire(#bind{target=Bid, type=click,postback=ReqKey}),
    nitro:wire(#bind{target=Suid,type=click,postback=Submit,source=Source}),
    nitro:wire(#bind{target=Soid,type=click,postback=Logout}),
    nitro:wire(#bind{target=Rid, type=click,postback=Init}),

    nitro:wire(#bind{target=Cid, type=error,    postback=OnError}),
    nitro:wire(#bind{target=Cid, type=session,  postback=OnSession}),
    nitro:wire(#bind{target=Cid, type=status,   postback=OnStatus}),
    nitro:wire(#bind{target=Cid, type=path,     postback=OnPath}),
    nitro:wire(#bind{target=Cid, type=login,    postback=OnLogin}),
    nitro:wire(#bind{target=Cid, type=ca,       postback=OnCa}),

    nitro:render(#panel{id=Cid, class=Class, body=[Title,Form]}).

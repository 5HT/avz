-module(cipher).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("proto.hrl").
-include("sdk.hrl").

-export([info/3, proc/2]).
-export([xhr/4]).

-define(API(S),         nitro:f("/api/v1~s",[S])).
-define(TICKET(Id,Cmd), ?API(nitro:f("/ticket/~s/~s", [Id,Cmd]))).
-define(STATUS,         ?API("/status")).
-define(SUPPORTED_CA,   ?API("/certificateAuthority/supported")).
-define(UI_CHOOSE_KEY,  ?API("/ui/keyContainerChooser")).
-define(SESSION,        ?API("/ticket")).
-define(SESSION(Id),    ?API(nitro:f("/ticket/~s", [Id]))).
-define(SESSION_OPT(Id),?TICKET(Id,"option")).
-define(SESSION_KEY(Id),?TICKET(Id,"keyStore")).
-define(SESSION_DTA(Id),?TICKET(Id,"data")).
-define(SESSION_SGN(Id),?TICKET(Id,"ds/creator")).
-define(SGN_BASE64(Id), ?TICKET(Id,"ds/base64Data")).
-define(VERIFIER(Id),   ?TICKET(Id,"ds/verifier")).
-define(SGN_DATA(Id),   ?TICKET(Id,"ds/data")).
-define(R(Msg), string:replace(Msg,"\"","&quot;",all)).

% cipher login protocol

% 1. sign random data
%   - create session
%   - set session data
%   - set session options
%   - set keystore path
%   - sign data with password
%   - check signature status
%   - retrieve signature data
%   - delete session

% 2. check signature
%   - create session
%   - set session options
%   - set session data
%   - request signature check
%   - retrieve signature
%   - delete session

% 3. check status
%  - fill the form data
%  - supported CAs (not sure if it has any sence) 
%  - key types hardcoded

% 4. protocol drivers
%   - nitro bases js client
%   - server based n2o cient
%   - driver switch? 

info(#cph{cid=Cid, cmd=init}=Cmd,R,#cx{req=#{pid:=Pid}}=S) ->
    catch n2o_pi:stop(async, Cid),
    case n2o_pi:start(#pi{module=?MODULE, table=async, sup=n2o, state=Cmd#cph{pid=Pid}, name=Cid}) of
        {error, E} -> {reply, {bert, E}, R,S};
        {_,_}  -> {reply, xhr("GET", {status, ?STATUS}, Cid), R,S} end;

info(#cph{cid=Cid,cmd=req_status}, R, S) ->
    {reply, xhr("GET", {status, ?STATUS}, Cid), R,S};

info(#cph{cid=Cid,cmd=req_auth},R,S) ->
    {reply, xhr("GET", {authorities, ?SUPPORTED_CA}, Cid), R,S};

info(#cph{cid=Cid,cmd=req_key},R,S) ->
    {reply, xhr("PUT", {file_path, ?UI_CHOOSE_KEY}, Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_session_data},R,S) ->
    Data = ?N2O_JSON:encode(#{<<"base64Data">> => base64:encode(crypto:strong_rand_bytes(16))}),

    {reply, xhr("POST", {session_data, ?SESSION_DTA(Tid)}, Data, Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_session_opt, pld=Opt}, R,S) ->
    {reply, xhr("PUT", {session_opt, ?SESSION_OPT(Tid)}, ?N2O_JSON:encode(Opt), Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_set_key, pld=Path},R,S) -> 
    {reply, xhr("PUT", {session_key, ?SESSION_KEY(Tid)}, ?N2O_JSON:encode(Path), Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_sign_data, pld=Pass}, R,S) ->
    {reply, xhr("POST", {signed_data, ?SESSION_SGN(Tid)}, ?N2O_JSON:encode(Pass), Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_signature}, R,S) ->
    {reply, xhr("GET", {signature_status, ?SESSION_SGN(Tid)},Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_signature_data}, R,S) ->
    {reply, xhr("GET", {signature_data, ?SGN_BASE64(Tid)}, Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_delete_session}, R,S) -> 
    {reply, xhr("DELETE", {delete_session,?SESSION(Tid)}, Cid), R,S};

info(#cph{cid=Cid, cmd=check_signature},R,S) ->
    {reply, xhr("POST", {session_verify, ?SESSION}, Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_verify_session_opt, pld=Opt}, R,S) ->
    {reply, xhr("PUT", {verify_session_opt, ?SESSION_OPT(Tid)}, ?N2O_JSON:encode(Opt), Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_verify_signature_data, pld=Data}, R,S) ->
    {reply, xhr("POST", {verify_signature_data, ?SGN_DATA(Tid)}, ?N2O_JSON:encode(Data), Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_signature_check}, R,S) ->
    {reply, xhr("POST", {signature_check, ?VERIFIER(Tid)}, "null", Cid, "text/plain"), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_signature_result}, R,S) ->
    {reply, xhr("GET", {signature, ?VERIFIER(Tid)},Cid), R,S};

info(#cph{cid=Cid, tid=Tid, cmd=req_verify_delete_session}, R,S) -> 
    {reply, xhr("DELETE", {verify_delete_session,?SESSION(Tid)}, Cid), R,S};

info(#cph{cid=Cid, cmd=submit, pld=Body},R,S) ->
    n2o_pi:cast(async,Cid,{submit, Body}),
    {reply, xhr("POST", {session, ?SESSION}, Cid), R,S};

info(#cph{cid=Cid, cmd=status, pld=Json}=Cph,R,#cx{req=#{pid:=Pid}}=S) ->
    n2o_pi:cast(async, Cid, Cph#cph{pid=Pid}),
    #{<<"message">>:=Msg} = ?N2O_JSON:decode(Json),

    {reply, io(nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('status', {detail: { html: () => '~s'}})", [Msg])]})), R, S};

info(#cph{cid=Cid, cmd=file_path, pld=Json}, R,S) ->
    #{<<"filePath">>:=Path} = ?N2O_JSON:decode(Json),

    {reply, io(nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('path', {detail: { html: () => '~s'}})", [Path])]})), R,S};

info(#cph{cid=Cid, cmd=authorities, pld=Json}=Cmd,R,S) ->
    #{<<"ca">>:=Auths} = ?N2O_JSON:decode(Json),
    n2o_pi:cast(async,Cid,Cmd#cph{pld=Auths}),

    {reply, io(
        nitro:wire(#jq{target=Cid,
            method=["dispatchEvent"],
            args=[nitro:f("new CustomEvent('ca', {detail: {ca: () => '~s'}})",[Json])]})
        % nitro:update(auth, #select{id=auth, class=field,
        %     body=lists:map(fun(#{<<"id">>:=Id, <<"name">>:=Name}) -> #option{label=?R(Name), value=Id} end, Auths) })
    ), R,S};

info(#cph{cid=Cid, cmd=session, pld=Json}, R,S) ->
    #{<<"ticketUuid">>:=Tiket} = ?N2O_JSON:decode(Json),
    n2o_pi:cast(async,Cid,{ticket, Tiket}),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid,cmd=session_data,pld=Json},R,S) ->
    n2o_pi:cast(async,Cid,session_data),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)) ,R,S};

info(#cph{cid=Cid, cmd=session_opt, pld=Json},R,S) ->
    n2o_pi:cast(async,Cid,session_opt),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=session_key, pld=Json},R,S) ->
    n2o_pi:cast(async,Cid,session_key),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=signed_data, pld=Json}, R,S) ->
    n2o_pi:cast(async,Cid,signed_data),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=signature_status,pld=Json}, R,S) ->
    n2o_pi:cast(async,Cid,signature_ok),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=signature_data, pld=Json}, R,S) ->
    M = jsone:decode(Json),
    M1 = maps:merge(#{<<"message">> => <<"Завершено виконання задачі &quot;Створення електронного підпису&quot;."/utf8>>}, M),
    
    case maps:find(<<"base64Data">>, M) of  error -> ok;
        {ok, Data} -> n2o_pi:cast(async,Cid,{signature_data,Data}) end,

    {reply, session_io(Cid,M1),R,S};

info(#cph{cid=Cid, cmd=delete_session, pld=Json}, R,S) ->
    n2o_pi:cast(async, Cid, delete_session),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

% copy-paste
info(#cph{cid=Cid, cmd=session_verify, pld=Json}, R,S) ->
    #{<<"ticketUuid">>:=Tiket} = ?N2O_JSON:decode(Json),
    n2o_pi:cast(async,Cid,{verify_ticket, Tiket}),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=verify_session_opt, pld=Json},R,S) ->
    n2o_pi:cast(async,Cid,verify_session_opt),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=verify_signature_data, pld=Json}, R,S) ->
    n2o_pi:cast(async,Cid,verify_signature_data),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=signature_check,pld=Json}, R,S) ->
    n2o_pi:cast(async,Cid,signature_check),
    Msg = case ?N2O_JSON:try_decode(Json) of
        {ok, M,_} -> M;
        {error,{Res, _}} -> #{<<"message">> => nitro:to_list(Res)}
    end,
    {reply, session_io(Cid,Msg),R,S};

info(#cph{cid=Cid, cmd=signature,pld=Json}, R,S) ->
    M = ?N2O_JSON:decode(Json),
    case maps:find(<<"verifyResults">>,M) of error -> ok;
        {ok,[Signature]} -> n2o_pi:cast(async,Cid,{signature,Signature})
    end,
    {reply, session_io(Cid,M),R,S};

info(#cph{cid=Cid, cmd=verify_delete_session, pld=Json}, R,S) ->
    n2o_pi:cast(async, Cid, verify_delete_session),
    {reply, session_io(Cid,?N2O_JSON:decode(Json)),R,S};

info(#cph{cid=Cid, cmd=login, pld=Signer},R,S) ->
    #{<<"ownerCertificateInfo">>:= #{<<"value">>:=Usr}} = Signer,
    #{<<"ownerFullName">> := #{<<"value">>:=Name}} = Usr,
    Msg = <<Name/binary, " успішно аутентифіковано"/utf8>>,
    nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('session', {detail: {html: () => '~s'}})", [?R(Msg)])]}
    ),

    nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('login', {detail: {}})")]}),
    {reply, io({login, Cid, Signer},S) ,R,S};

info(#cph{cid=Cid,cmd=logout}, R,S) ->
  n2o:user([]),
  n2o_session:delete({n2o:sid(),token}),
  {reply,{bert,nitro_n2o:io({logout,Cid},S)},R,S};

info(#cph{cid=Cid, cmd=error, pld=Fail},R,S) ->
    catch n2o_pi:stop(async, Cid),

    nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('error', {detail: {data: () => '~s'}})", [Fail])] }),

    {reply, io({error,Cid,Fail}), R,S};

info(M,R,S) -> io:format("[cipher] skip: ~p~n", [M]), {unknown, M, R, S}.

% pi state of protocol

proc(init, #pi{state=S}=Pi) ->
    % application:get_env session options
    {ok, Pi#pi{state=S#cph{tmr=erlang:start_timer(5000, self(), status)}}};

proc({timeout,Ref,status}, #pi{state=#cph{pid=Pid,tmr=Ref}=S}=Pi) when is_pid(Pid) ->
    Pid ! S#cph{cmd=req_status},
    {noreply, Pi#pi{state=S#cph{tmr=erlang:start_timer(5000, self(), status)}}};

proc({timeout,_,_},#pi{}=Pi) -> {stop, normal,Pi};

proc(#cph{cmd=status, pid=Pid},#pi{state=S}=Pi) ->
    case get(authorities) of undefined -> Pid ! S#cph{cmd=req_auth}; _ -> skip end,
    
    {noreply, Pi#pi{state=S#cph{pid=Pid}}};

proc(#cph{cmd=authorities, pld=CA},#pi{}=Pi) -> put(authorities, CA), {noreply,Pi};

proc({submit,[{ca,Ca},{type,Type},{key,Key},{pass,Pass}]}, #pi{}=Pi) ->
    put(ca, Ca), put(type, Type), put(key, Key), put(pass,Pass),
    {noreply, Pi};

proc({ticket, Tid}, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    Pid ! S#cph{tid=Tid, cmd=req_session_data},
    {noreply,Pi#pi{state=S#cph{tid=Tid}}};

proc(session_data, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    Opt = #{ <<"caId">> => get(ca)
           , <<"cadesType">> => <<"CAdESXLong">>
           , <<"signatureType">> => <<"attached">>
           , <<"dataToSignQualifier">> => <<"notSignedBefore">>
           },
    Pid ! S#cph{cmd=req_session_opt, pld=Opt},
    {noreply, Pi};
proc(session_opt, #pi{state=#cph{pid=Pid}=S}=Pi)  ->
    Pid ! S#cph{cmd=req_set_key, pld=#{<<"keyStorePath">> => get(key)}},
    {noreply, Pi};
proc(session_key, #pi{state=#cph{pid=Pid}=S}=Pi)  ->
    Pid ! S#cph{cmd=req_sign_data, pld=#{<<"keyStorePassword">> => get(pass)}},
    {noreply, Pi};
proc(signed_data, #pi{state=#cph{pid=Pid}=S}=Pi)  -> Pid ! S#cph{cmd=req_signature}, {noreply, Pi};
proc(signature_ok, #pi{state=#cph{pid=Pid}=S}=Pi) -> Pid ! S#cph{cmd=req_signature_data}, {noreply, Pi};
proc({signature_data,Data}, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    put(data, Data),
    Pid ! S#cph{cmd=req_delete_session},
    {noreply,Pi};
proc(delete_session, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    %  check signature on remote host here
    Pid ! S#cph{cmd=check_signature},
    {noreply,Pi};

proc({verify_ticket, Tid}, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    Opt = #{ <<"cadesType">> => <<"CAdESXLong">>
           , <<"signatureType">> => <<"attached">>},
    Pid ! S#cph{tid=Tid, cmd=req_verify_session_opt, pld=Opt},
    {noreply,Pi#pi{state=S#cph{tid=Tid}}};

proc(verify_session_opt, #pi{state=#cph{pid=Pid}=S}=Pi)  ->
    Pid ! S#cph{cmd=req_verify_signature_data, pld=#{<<"base64Data">> => get(data)}},
    {noreply, Pi};

proc(verify_signature_data, #pi{state=#cph{pid=Pid}=S}=Pi)  -> Pid ! S#cph{cmd=req_signature_check}, {noreply, Pi};
proc(signature_check, #pi{state=#cph{pid=Pid}=S}=Pi) -> Pid ! S#cph{cmd=req_signature_result}, {noreply, Pi};

proc({signature,#{<<"status">>:= <<"FAILURE">>, <<"failureCause">>:= Fail}}, #pi{state=#cph{pid=Pid}=S}=Pi)->
    Pid ! S#cph{cmd=req_verify_delete_session},
    Pid ! S#cph{cmd=error, pld=Fail},
    {noreply,Pi};

proc({signature,#{<<"status">>:= <<"SUCCESS">> , <<"signerInfo">>:=Signer}}, #pi{state=#cph{pid=Pid}=S}=Pi) ->
    Pid ! S#cph{cmd=req_verify_delete_session},
    Pid ! S#cph{cmd=login, pld=Signer},
    {noreply, Pi};

proc(verify_delete_session, #pi{}=Pi) -> {stop,normal,Pi};

proc(_,#pi{}=Pi) -> {reply, ok, Pi}.

% nitro, xhr over nitro

io(Actions)     -> {bert, nitro_n2o:io(Actions) }.
io(Actions,Ctx) -> {bert, nitro_n2o:io(Actions,Ctx)}.

session_io(Cid, #{<<"message">>:=Msg}) -> 
    io(nitro:wire(#jq{target=Cid,
        method=["dispatchEvent"],
        args=[nitro:f("new CustomEvent('session', {detail: {html: () => '~s'}})", [?R(Msg)])]}
    )).

xhr(M, C, Cid) -> xhr(M, C, "null", Cid).
xhr(M, {C,Cmd}, Data, Cid) -> xhr(M, {C,Cmd}, Data, Cid, "application/json").
xhr(M, {C,Cmd}, Data, Cid, CT) ->
    Url = nitro:f("~s~s", [?CIPHER_URL,Cmd]),
    Ret = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,C,"bin(x.response)")]),
    Err = nitro:f("ws.send(~s);", [?AVZ(cph,Cid,error,"bin(x.response||x.status)")]),

    Data1 = case CT of "application/json" when Data =/= "null" -> nitro:f("JSON.stringify(~s)",[Data]);_ -> Data end,

    io(nitro:wire(iolist_to_binary([
    "(function(){",
        "let x = new XMLHttpRequest();",
        "x.open('",M,"','",Url,"', true);",
        "x.setRequestHeader('Access-Control-Allow-Origin',window.location.origin);",
        "x.setRequestHeader('Content-Type', '",CT,"');",
        "x.onload = function() {",Ret,"};",
        "x.onerror= function() {",Err,"};",
        "x.send(",Data1,");",
    "})();" ]))).

%
% Nitro subprotocols to handle sdk events
%
-define(AVZ , cid = [] :: [] | binary()    %% control id
            , sid = [] :: [] | binary()    %% session id
            , pid = [] :: [] | reference() %% ws pid
            , tid = [] :: [] | binary()    %% ticket/ext id
            , cmd = [] :: [] | atom()      %% command
            , tmr = [] :: [] | reference() %% timer
            , pld = [] :: [] | term()      %% payload
            ).

-define(AVZ(Type,Cid,Cmd), ?AVZ(Type,Cid,Cmd,"bin('')")).
-define(AVZ(Type,Cid,Cmd,Payload), iolist_to_binary([
  "enc(tuple("
  , "atom('",atom_to_list(Type),"'),"
  , "bin('",Cid,"'),"
  , "bin(token()),"
  , "bin(''),"
  , "bin(''),"
  , "atom('",atom_to_list(Cmd),"'),"
  , "bin(''),"
  ,  Payload
  , "))" ])).

-record(avz, { ?AVZ }).
-record(eml, { ?AVZ }).
-record(cph, { ?AVZ }).

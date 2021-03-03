-ifndef(USR_HRL).
-define(USR_HRL, true).
%-include_lib("kvs/incude/").

-record(user,   { id = kvs:seq([],[]) :: term()
                , email = []
                , username = []
                , password = []
                , display_name = []
                , register_date = []
                , tokens = []
                , images = []
                , names = []
                , surnames =[]
                , birth = []
                , sex = []
                , status = []
                , type = [] 
                }).

-endif.
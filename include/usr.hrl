-ifndef(USR_HRL).
-define(USR_HRL, true).

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
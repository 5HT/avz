-record(struct, {lst=[]}).
-define(AFTER_LOGIN, "/account").
-define(LOGIN_PAGE, "/login").
-define(METHODS, [facebook,google,github,twitter]).
-define(API,[sdk/0,               % JavaScript for page embedding for JavaScript based login methods
             login_button/0,      % HTML Button for page embedding
             event/1,             % Page Event for HTTP redirect based login methods
             api_event/3,         % Page Event for JavaScript based login methods
             callback/0,          % Callback part of HTTP redirect based login methods
             registration_data/3  % Process Parameters
            ]).

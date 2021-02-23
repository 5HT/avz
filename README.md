AVZ Login System for N2O
========================

Authentication methods for N2O sites. Supports both JavaScript based
logins (like Google and Facebook) and redirect based OAuth logins (Twitter, Github)
in a sane and simple manner. Compatible with but not limited to Nitrogen and N2O.

Supported Methods
-----------------

* Twitter
* Google
* Facebook
* Github
* Microsoft
* Telegram

API
---

    sdk/0                % JavaScript for page embedding for JavaScript based login methods
    login_button/0       % HTML Button for page embedding
    event/1              % Page Event for HTTP redirect based login methods
    api_event/3          % Page Event for JavaScript based login methods
    callback/0           % Callback part of HTTP redirect based login methods
    registration_data/3  % Process Parameters

Styles
------

Library provide only basic HTML markup for the buttons where its not strictly regulated by provider design guidelines.
However its easy to provide custom CSS class and/or support any CSS framework on your login page with N2O `#jq{}` action.

Example of styling FB login button with Bootstrap:

```erlang
event(init) -> 
  wf:wire(#jq{target=loginfb, method=["classList.add"], args=["'btn', 'btn-primary', 'btn-lg'"]}),
  ...
```

Example of styling Github button with Pure.css

```erlang
event(init) ->
  wf:wire(#jq{target=github_btn, method=["classList.add"], args=["'pure-button'"]})
  ...
```

Configuration
-------------

Authentication endpoints can be configured in your `sys.config` under avz application settings.

Available settings listed below with test applications configured for each provider and will
call you back on `http://localhost:8000/login`.


```erlang
{
  ...
  {avz, [ % General
        {after_login_page, "/index"},
        {login_page, "/login"},
        {json,jsone},

        % Facebook Login
        {fb_id, "176025532423202"},

        % Twitter OAuth
        {tw_consumer_key, "YwfU5qj5AYY0uwPumcw1Q"},
        {tw_consumer_secret, "O7VjRYLWxwMgtSXZbiiY6kc1Og2il9gbo1KAIqZk"},

        % Google Sign-In
        {g_client_id, "158344909038-j6c0rbvpi09kdaqq03j2eddlf047ht3d.apps.googleusercontent.com"}, 
        {g_cookiepolicy, "http://localhost:8000"},
        
        % Google Sign-In button settings
        {g_btn_width, 240},
        {g_btn_height, 50},
        {g_btn_theme, "light"},
        {g_btn_longtitle, true},

        % Github OAuth
        {github_client_id, "591bfe2556ee60ca8c32"},
        {github_client_secret, "01411884e3c51624d3ea729ed6b047db52973e8e"},

        % Microsoft Account Login
        {ms_client_id, "54385d15-f1e0-4fcf-9bf4-042d740e0df4"},
        {ms_client_secret, "jU0tStEzRdDPFwL9NdVGYxo"},
        {ms_redirect_uri, "http://localhost:8000/login"},
        
        % Telegram Login Widget
        {tl_bot, "NYNJA_bot"},
        {tl_bot_token, "548231922:AAHmXMMr38XGtH0tJMDUdiByheT2mZ7qkVI"}
        {tl_auth_url, "http://127.0.0.1/login"},
        {tl_request_access, "write"}
        {tl_btn_size, "large"},
        {tl_btn_radius, "20"},
  ]}
  ...
}
```

Telegram Notes
---------------
Login widget is displayed within the iframe, so the battle of `CPS` and `x-frame-options` is expected in different browsers.

When setting a domain in BotFather with `/setdomain`, please note that telegram will cut the port part of your domain in the `X-Frame-Options` and `Content-Security-Policy` response headers. 

So in fact you are limited to use 80 and 443 ports only.

Credits
-------

* Andrii Zadorozhnii
* Andrii Sergiienko
* Maxim Sokhatsky

OM A HUM

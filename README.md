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

API
---

    sdk/0                % JavaScript for page embedding for JavaScript based login methods
    login_button/0       % HTML Button for page embedding
    event/1              % Page Event for HTTP redirect based login methods
    api_event/0          % Page Event for JavaScript based login methods
    callback/0           % Callback part of HTTP redirect based login methods
    registration_data/3  % Process Parameters

Credits
-------

* Andrii Zadorozhnii
* Maxim Sokhatsky

OM A HUM

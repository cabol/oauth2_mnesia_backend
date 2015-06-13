# OAuth2 Example using Mnesia backend

> **NOTE**: This example is based on [oauth2_example](https://github.com/kivra/oauth2_example).

This is an example application intended to demonstrate how the Kivra OAuth2
library can be used with **Mnesia** backend support.

# Walkthrough

## Getting started

To build and start the example:

    $ make

Once build finishes and release folder `_rel` is created, you're able to start the app:

    $ ./_rel/oauth2_example/bin/oauth2_example console

Provided all goes well you now have a running Cowboy listener on port 8080.

## Getting authorization

We'll start off by setting up a user account. Switch to the Erlang shell
you just started and do:

    1> oauth2_mnesia_backend:add_user(<<"bob">>, <<"luv_alice">>).
    ok

Now that we have a user set up, we can authenticate through the
*Resource Owner Password Credentials* grant type. Release your inner cURL:

    $ curl -v -X POST http://127.0.0.1:8080/auth \
        -d "grant_type=password&username=bob&password=luv_alice&scope=yourbase"
    < HTTP/1.1 200 OK
    < connection: keep-alive
    * Server Cowboy is not blacklisted
    < server: Cowboy
    < date: Fri, 12 Jun 2015 23:36:35 GMT
    < content-length: 133
    < content-type: application/json
    < vary: accept
    {
        "access_token": "1t4VDlrqpgzih1OEd6lkoMkSndeA9p1y",
        "expires_in": "3600",
        "scope": "yourbase",
        "token_type": "bearer"
    }

The `access_token` field is the critical part. When issuing a request for
a resource, we have two ways of providing the token to the server:
via the `Authorization` header, or via the `access_token` query string
parameter. The latter should be quite self-explanatory, so we'll do the former
instead:

    $ curl -v -H "Authorization: Bearer 1t4VDlrqpgzih1OEd6lkoMkSndeA9p1y" \
        http://127.0.0.1:8080/resource
    < HTTP/1.1 200 OK
    < connection: keep-alive
    * Server Cowboy is not blacklisted
    < server: Cowboy
    < date: Sat, 13 Jun 2015 01:38:59 GMT
    < content-length: 0
    < content-type: application/json

Seems to have worked well enough; the 204 response indicates that no content
is available at this URL, which is all well, since no content is served.
Had we omitted the access token, or provided an invalid or expired token,
we would have been rejected with an HTTP 401 instead. Observe:

    $ curl -v http://127.0.0.1:8080/resource
    < HTTP/1.1 401 Unauthorized
    < connection: keep-alive
    * Server Cowboy is not blacklisted
    < server: Cowboy
    < date: Sat, 13 Jun 2015 01:40:49 GMT
    < content-length: 0
    < www-authenticate: Bearer

# Other grant types

## Client Credentials

The OAuth2 library also supports the *Client Credentials* grant type,
in which an access token is issued for a *client* rather than a *user*;
no username or password is needed; the client simply authenticates
with its own identifier and secret. It should, for obvious reasons, not be
used with clients that are distributed to end users, since it's probably
easy enough to find the client secret in the distributed binary, thus
compromising your security.

The *Client Credentials* grant works in a fashion quite similar to that
of the *Resource Owner Password Credentials* grant, with the key difference
being that the client supplies its credentials as an HTTP Basic Auth
header of the form

    Authorization: Basic base64(client_id + ":" + client_secret)

For instance, a client with the identifier `my_client` and secret `souper_sekr3t`
would identify with:

    Authorization: Basic bXlfY2xpZW50OnNvdXBlcl9zZWtyM3Q=

Start off by registering the client the same way we registered the user:

    2> oauth2_mnesia_backend:add_client(<<"my_client">>, <<"souper_sekr3t">>).
    ok

However, as with the *Resource Owner Password Credentials* grant type,
you also need to provide a scope and a grant type parameter.
This is done in an `application/x-www-form-urlencoded` body as before:

    $ curl -v -X POST \
        -H "Authorization: Basic bXlfY2xpZW50OnNvdXBlcl9zZWtyM3Q=" \
        -d "grant_type=client_credentials&scope=yourbase"
        http://127.0.0.1:8080/auth
    < HTTP/1.1 200 OK
    < connection: keep-alive
    * Server Cowboy is not blacklisted
    < server: Cowboy
    < date: Sat, 13 Jun 2015 01:42:34 GMT
    < content-length: 110
    < content-type: application/json
    < vary: accept
    <
    {
        "access_token": "a4QJhx31xCJI6kaqsg4WPNWJkxYBYyEh",
        "expires_in": "3600",
        "scope": "yourbase",
        "token_type": "bearer"
    }

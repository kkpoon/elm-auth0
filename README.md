# elm-auth0

Auth0 for elmlang

This elm package provides data types and helper function for elm web application
to use [Auth0](https://auth0.com/).

## Example

To create an URL for your web app "Login with Google", the following
URL builder could help.

```elm
auth0AuthorizeURL
    (Auth0Config "https://my-app.auth0.com" "aBcD1234")
    "token"
    "https://my-app/"
    [ "openid", "name", "email" ]
    (Just "google-oauth2")
    Nothing
```

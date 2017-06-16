module Auth0
    exposing
        ( Endpoint
        , IdToken
        , UserID
        , Auth0Config
        , Profile
        , profileDecoder
        , OAuth2Identity
        , auth0AuthorizeURL
        , getAuthedUserProfile
        , updateUserMetaData
        )

{-| This library provides data types and helper functions for [Auth0](https://auth0.com)


# Auth0 Basis

@docs Endpoint, IdToken, UserID, Auth0Config


# User Profile

@docs Profile, OAuth2Identity, profileDecoder


# Helpers

@docs auth0AuthorizeURL, getAuthedUserProfile, updateUserMetaData

-}

import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode as Decode exposing (Decoder, maybe, list, string, bool)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode


{-| Auth0 Endpoint of Authentication API and Management API

e.g. <https://your-auth0-app.auth0.com>

-}
type alias Endpoint =
    String


{-| The idToken returns from Auth0 authenticated result, it is usually in JSON
Web Token ([JWT](https://jwt.io)) format.
-}
type alias IdToken =
    String


{-| Represent the Auth0 unified user ID
-}
type alias UserID =
    String


{-| A config record of Auth0
-}
type alias Auth0Config =
    { endpoint : Endpoint
    , clientId : String
    }


{-| Auth0 unified user profile

The `user_metatdata` and `app_metadata` varies in different application.
You should define your own `user_metadata` and `app_metadata` records.

-}
type alias Profile userMetaData appMetaData =
    { email : String
    , email_verified : Bool
    , family_name : String
    , given_name : String
    , global_client_id : Maybe String
    , identities : List OAuth2Identity
    , locale : Maybe String
    , name : String
    , nickname : String
    , picture : String
    , user_id : UserID
    , user_metadata : Maybe userMetaData
    , app_metadata : Maybe appMetaData
    }


{-| Auth0 unified user profile decoder

The `user_metatdata` and `app_metadata` varies in different application.
You should define your own `user_metadata` and `app_metadata` decoders.

-}
profileDecoder : Decoder a -> Decoder b -> Decoder (Profile a b)
profileDecoder a b =
    decode Profile
        |> required "email" string
        |> required "email_verified" bool
        |> required "family_name" string
        |> required "given_name" string
        |> optional "global_client_id" (maybe string) Nothing
        |> required "identities" (list oAuth2IdentityDecoder)
        |> optional "locale" (maybe string) Nothing
        |> required "name" string
        |> required "nickname" string
        |> required "picture" string
        |> required "user_id" string
        |> optional "user_metadata" (maybe a) Nothing
        |> optional "app_metadata" (maybe b) Nothing


{-| The OAuth2 identity of the unified user profile. This usually tell the
social account or database account linked with the unified user profile.
-}
type alias OAuth2Identity =
    { connection : String
    , isSocial : Bool
    , provider : String
    , user_id : String
    }


oAuth2IdentityDecoder : Decoder OAuth2Identity
oAuth2IdentityDecoder =
    decode OAuth2Identity
        |> required "connection" string
        |> required "isSocial" bool
        |> required "provider" string
        |> required "user_id" string


{-| Create the URL to the login page

    auth0AuthorizeURL :
        Auth0Config
        -> String -- responseType
        -> String -- redirectURL
        -> List String -- scopes
        -> Maybe String -- connection
        -> String

e.g.

    auth0AuthorizeURL
        (Auth0Config "https://my-app.auth0.com" "aBcD1234")
        "token"
        "https://my-app/"
        [ "openid", "name", "email" ]
        (Just "google-oauth2")

-}
auth0AuthorizeURL :
    Auth0Config
    -> String
    -> String
    -> List String
    -> Maybe String
    -> String
auth0AuthorizeURL auth0Config responseType redirectURL scopes maybeConn =
    let
        connectionParam =
            maybeConn
                |> Maybe.map (\c -> "&connection=" ++ c)
                |> Maybe.withDefault ""

        scopeParam =
            scopes |> String.join " " |> Http.encodeUri
    in
        auth0Config.endpoint
            ++ "/authorize"
            ++ ("?response_type=" ++ responseType)
            ++ ("&client_id=" ++ auth0Config.clientId)
            ++ connectionParam
            ++ ("&redirect_uri=" ++ redirectURL)
            ++ ("&scope=" ++ scopeParam)


{-| Get the Auth0 unified user profile which is represented by the `IdToken`
-}
getAuthedUserProfile :
    Endpoint
    -> IdToken
    -> Decoder profile
    -> Request profile
getAuthedUserProfile auth0Endpoint idToken profileDecoder =
    Http.request
        { method = "POST"
        , headers = []
        , url = auth0Endpoint ++ "/tokeninfo"
        , body =
            Http.jsonBody <|
                Encode.object [ ( "id_token", Encode.string idToken ) ]
        , expect = Http.expectJson profileDecoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Update the `user_metadata` in the Auth0 unified user profile
-}
updateUserMetaData :
    Endpoint
    -> IdToken
    -> UserID
    -> Encode.Value
    -> Decoder profile
    -> Request profile
updateUserMetaData auth0Endpoint idToken userID userMeta profileDecoder =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ idToken) ]
        , url = auth0Endpoint ++ "/api/v2/users/" ++ userID
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "user_metadata", userMeta ) ]
        , expect = Http.expectJson profileDecoder
        , timeout = Nothing
        , withCredentials = False
        }

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

import Http exposing (..)
import Iso8601
import Json.Decode as Decode exposing (Decoder, maybe, list, string, bool, field)
import Json.Encode as Encode
import Parser
import Time
import Url


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
    , created_at : Time.Posix
    , family_name : Maybe String
    , given_name : Maybe String
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
    Decode.map7
        (<|)
        (Decode.map8 Profile
            (field "email" string)
            (field "email_verified" bool)
            (field "created_at" dateDecoder)
            (maybe (field "family_name" string))
            (maybe (field "given_name" string))
            (maybe (field "global_client_id" string))
            (field "identities" (list oAuth2IdentityDecoder))
            (maybe (field "locale" string))
        )
        (field "name" string)
        (field "nickname" string)
        (field "picture" string)
        (field "user_id" string)
        (maybe (field "user_metadata" a))
        (maybe (field "app_metadata" b))


dateDecoder : Decoder Time.Posix
dateDecoder =
    let
        dateStringDecode dateString =
            case Iso8601.toTime dateString of
                Result.Ok date ->
                    Decode.succeed date

                Err errorMessage ->
                    Decode.fail (Parser.deadEndsToString errorMessage)
    in
        Decode.string |> Decode.andThen dateStringDecode



type Msg
  = GotProfile (Result Http.Error String)

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
    Decode.map4 OAuth2Identity
        (field "connection" string)
        (field "isSocial" bool)
        (field "provider" string)
        (field "user_id" string)


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
    -> Maybe String
    -> String
auth0AuthorizeURL auth0Config responseType redirectURL scopes maybeConn maybeAud =
    let
        connectionParam =
            maybeConn
                |> Maybe.map (\c -> "&connection=" ++ c)
                |> Maybe.withDefault ""
        audParam =
          maybeAud
            |> Maybe.map (\aud -> "&audience=" ++ aud)
            |> Maybe.withDefault ""

        scopeParam =
            scopes |> String.join " " |> Url.percentEncode
    in
        auth0Config.endpoint
            ++ "/authorize"
            ++ ("?response_type=" ++ responseType)
            ++ ("&client_id=" ++ auth0Config.clientId)
            ++ connectionParam
            ++ audParam
            ++ ("&redirect_uri=" ++ redirectURL)
            ++ ("&scope=" ++ scopeParam)


{-| Get the Auth0 unified user profile which is represented by the `IdToken`
-}
getAuthedUserProfile :
    Endpoint
    -> IdToken
    -> Decoder String
    -> Cmd Msg
getAuthedUserProfile auth0Endpoint idToken pDecoder =
    Http.request
        { method = "POST"
        , headers = []
        , url = auth0Endpoint ++ "/tokeninfo"
        , body =
            Http.jsonBody <|
                Encode.object [ ( "id_token", Encode.string idToken ) ]
        , expect = Http.expectJson GotProfile pDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Update the `user_metadata` in the Auth0 unified user profile
-}
updateUserMetaData :
    Endpoint
    -> IdToken
    -> UserID
    -> Encode.Value
    -> Decoder String
    -> Cmd Msg
updateUserMetaData auth0Endpoint idToken userID userMeta pDecoder =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ idToken) ]
        , url = auth0Endpoint ++ "/api/v2/users/" ++ userID
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "user_metadata", userMeta ) ]
        , expect = Http.expectJson GotProfile pDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

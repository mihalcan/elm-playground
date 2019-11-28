module Main exposing (..)

import Browser
import Char exposing (isDigit, isLower, isUpper)
import Debug exposing (log)
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type Validation
    = None
    | Error String
    | Ok


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , valid : Validation
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , password = ""
      , passwordAgain = ""
      , valid = None
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Check


validate : Model -> Validation
validate model =
    if model.password /= model.passwordAgain then
        Error "Passwords don't match"

    else if String.length model.password < 8 then
        Error "Password must be 8 characters or more"

    else if not (String.any isDigit model.password) then
        Error "Password must contain digits"

    else if not (String.any isUpper model.password) then
        Error "Password must contain uppercase"

    else if not (String.any isLower model.password) then
        Error "Password must contain lowercase"

    else
        Ok


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordAgain password ->
            ( { model | passwordAgain = password }, Cmd.none )

        Check ->
            ( { model | valid = validate model }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model.valid
        , button [ onClick Check ] [ text "Submit" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Validation -> Html msg
viewValidation valid =
    let
        ( colour, txt ) =
            case valid of
                None ->
                    ( "black", "Please enter your details" )

                Error error ->
                    ( "red", error )

                Ok ->
                    ( "green", "OK" )
    in
    div [ style "color" colour ] [ text txt ]


validatePassword : Model -> String
validatePassword model =
    if String.length model.password < 8 then
        "Password must be minimum 8 characters"

    else
        ""

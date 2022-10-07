module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)



main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }



-- MODEL --

type alias Model =
  { format : String
  , result : String
  }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { format = "カニクリームコロッケ"
    , result = ""
    }
  , Cmd.none
  )



-- UPDATE --

type Msg
  = FormatUpdate String
  | Reroll
  | Rolled (List Char)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ format, result } as model) =
  case msg of
    FormatUpdate str ->
      ( Model str result
      , Cmd.none
      )

    Reroll ->
      ( model
      , Random.generate Rolled
          <| randomList
          <| getSortee format
      )

    Rolled list ->
      let
        result_ =
          construct format list
      in
        ( Model format result_
        , Cmd.none
        )

getSortee : String -> List Char
getSortee format =
  String.toList format

construct : String -> List Char -> String
construct format list =
  String.fromList list



-- VIEW --

view : Model -> Html Msg
view { format, result } =
  div [ class "contents" ]
    [ div []
        [ input [ onInput FormatUpdate, value format ] []
        , button [ onClick Reroll ] [ text "生成" ]
        ]
    , div []
      [ text <| result ]
    ]



-- RANDOM --

randomList : List a -> Generator (List a)
randomList original =
  Random.independentSeed
    |> Random.map (\initSeed ->
        original
          |> List.foldl
              (\e ( list, seed ) ->
                  let
                    ( rank, nextSeed ) =
                      seed |> Random.step anyInt -- ignore minor biases
                  in
                    ( ( e, rank ) :: list, nextSeed ))
              ( [], initSeed )
          |> (\( list, _ ) -> list)
          |> List.sortBy (\( _, rank ) -> rank)
          |> List.map (\( e, _ ) -> e))

anyInt : Generator Int
anyInt =
  Random.int Random.minInt Random.maxInt
module Main exposing (..)

--import Html.Styled exposing (Html, a, button, div, li, p, strong, text, ul)
--import Html.Attributes exposing (href)

import Basics
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Round


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



-- MODEL


type PowerUseUnit
    = DrinksCan
    | IPhone6
    | Mile
    | CupOfTea
    | WashingCycle
    | Hour
    | Minute
    | PopcornBag


unitName : PowerUseUnit -> String
unitName x =
    case x of
        Hour ->
            "hour"

        Minute ->
            "minute"

        PopcornBag ->
            "bag"

        DrinksCan ->
            "330ml drinks can"

        WashingCycle ->
            "cycle of a "

        CupOfTea ->
            "cup"

        Mile ->
            "mile"

        IPhone6 ->
            "iPhone 6"


type alias EnergyUse =
    { label : String
    , useUnit : PowerUseUnit
    , wattHours : Float
    , info : String
    }


type alias Model =
    { selected : EnergyUse
    , energyUses : List EnergyUse
    }


type alias Entry =
    { energyUse : EnergyUse
    , num : Float
    , unitName : String
    }


makingDrinksCan =
    EnergyUse "made from new aluminium" DrinksCan 140 "recycled aluminium would require only 5% of the energy"


lowEnergyTV =
    EnergyUse "watching TV" Hour 50 "A++ TV consuming 50w an hour"


ledBulb =
    EnergyUse "lighting an LED spotlight" Hour 5 "5w spot light, providing 346 lumens, equivalent to a 50w incandescent"


fridgeFreezer =
    EnergyUse "cooling a fridge-freezer" Hour 18.26 "based on a Miele A+++ 300l Fridge-Freezer"


ebike =
    EnergyUse "riding an e-bike" Mile 3.9 "based on a Bosch ActiveLine in eco-mode"


electricCar =
    EnergyUse "driving an electric car" Mile 250 "based on a Nissan Leaf: a Tesla Model S will go 72% of this distance; a Renault Zoe will go 38% further!"


init : Model
init =
    Model electricCar
        [ makingDrinksCan
        , lowEnergyTV
        , ledBulb
        , fridgeFreezer
        , ebike
        , electricCar
        , EnergyUse "of tea" CupOfTea 33 "boiling the water in a normal 3kw kettle"
        , EnergyUse "of popcorn" PopcornBag 27 "popped in an 800w microwave"
        , EnergyUse "cooking on an induction hob" Minute 33.333 "assuming the hob is on full power"
        , EnergyUse "taking a shower" Minute 158.3333 "(electric shower 9.5kw)"
        , EnergyUse "vacuuming" Minute 15 "based on a 900w compact vacuum cleaner"
        , EnergyUse "charging to full" IPhone6 22 ""
        , EnergyUse "using a laptop" Hour 58.4 "based on an Apple MacBook Pro 15\" (2.9 GHz)"
        , EnergyUse "washing machine on Baby Care (60ºC)" WashingCycle 2200 ""
        ]



-- UPDATE


type alias Msg =
    EnergyUse


update : Msg -> Model -> Model
update msg model =
    { model | selected = msg }


roundish : Float -> String
roundish n =
    if n < 0.9 then
        -- Round.round 2 n
        Round.round 0 (n * 100) ++ "%"
    else if n < 9 then
        Round.round 1 n
    else
        Round.round 0 n


makeEntry selectedWattHours energyUse =
    let
        num =
            selectedWattHours / energyUse.wattHours
    in
    case energyUse.useUnit of
        Hour ->
            if num < (1 / 60) then
                Entry energyUse (num * 60 * 60) "second"
            else if num < 1 then
                Entry energyUse (num * 60) "minute"
            else if num >= 24 then
                Entry energyUse (num / 24) "day"
            else
                Entry energyUse num "hour"

        Minute ->
            if num < 1 then
                Entry energyUse (num * 60) "second"
            else if num >= (60 * 24) then
                Entry energyUse (num / (60 * 24)) "day"
            else if num >= 60 then
                Entry energyUse (num / 60) "hour"
            else
                Entry energyUse num "minute"

        _ ->
            Entry energyUse num (unitName energyUse.useUnit)



-- VIEW


view : Model -> Html Msg
view model =
    div [ css [ color (rgb 14 15 16), fontSize (pt 22), fontFamilies [ "Impact", "Trebuchet MS", "sans-serif" ] ] ]
        [ p
            []
            [ bigStrong "1 "
            , text (unitName model.selected.useUnit ++ " ")
            , strong [ css [ fontSize larger, fontWeight lighter, color (hex "#5779DD"), padding (Css.em 0.25) ] ] [ text model.selected.label ]
            , text " uses the same energy as:"
            ]
        , div []
            (model.energyUses
                |> List.filter (\eu -> eu /= model.selected)
                |> List.map (makeEntry model.selected.wattHours)
                |> List.sortBy .num
                |> List.reverse
                |> List.map
                    (\entry ->
                        div [ css [] ]
                            [ bigStrong (roundish entry.num)
                            , text
                                ((if entry.num < 1 then
                                    " of a "
                                  else
                                    " "
                                 )
                                    ++ entry.unitName
                                    ++ (if entry.num < 1 then
                                            " "
                                        else
                                            "s "
                                       )
                                )
                            , a
                                [ onClick entry.energyUse, href "#", css [ color (hex "#5779B7") ] ]
                                [ text entry.energyUse.label ]
                            ]
                    )
            )
        ]


bigStrong txt =
    strong [ css [ fontSize (pt 38), color (hex "#5779A8"), padding (Css.em 0.25) ] ] [ text txt ]

module Main exposing (..)

--import Html.Styled exposing (Html, a, button, div, li, p, strong, text, ul)
--import Html.Attributes exposing (href)

import Basics
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, min, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
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
    | Bitcoin
    | Gigabyte


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
            "cycle"

        CupOfTea ->
            "cup"

        Mile ->
            "mile"

        IPhone6 ->
            "iPhone 6"

        Bitcoin ->
            "bitcoin"

        Gigabyte ->
            "gigabyte"


type alias EnergyUse =
    { label : String
    , useUnit : PowerUseUnit
    , wattHours : Float
    , info : String
    , icon : String
    }


type alias Model =
    { selected : EnergyUse
    , energyUses : List EnergyUse
    , quantity : Int
    }


type alias Entry =
    { energyUse : EnergyUse
    , num : Float
    , unitName : String
    }


electricCar : EnergyUse
electricCar =
    EnergyUse "driving an electric car" Mile 250 "Car energy efficiency figures based on a Nissan Leaf. A Tesla Model S will go 72% of this distance; a Renault Zoe will go 38% further! A 45pmg petrol car will get only 25% of this distance" "car"


init : Model
init =
    Model electricCar
        [ electricCar
        , EnergyUse "lighting an LED spotlight" Hour 5 "5w spot light, providing 346 lumens, equivalent to a 50w incandescent" "bulb"
        , EnergyUse "made from new aluminium" DrinksCan 140 "Recycled aluminium would require only 5% of the energy" "can"
        , EnergyUse "cooling a fridge-freezer" Hour 18.26 "With a Miele A+++ 300l Fridge-Freezer. A modern fridge uses between 40 and 60% less energy than models sold before 2000. An A+ model uses 20% less energy than an A-rated fridge-freezer. An A++ uses about 40% less. Stacked fridge-freezers are about 20% more efficient than American-style side-stacked models." "fridge"
        , EnergyUse "riding an e-bike" Mile 10.989 "With a Bosch Active Line drive system, used in \"Turbo\" mode, with an average speed of 14mph. In \"eco\" mode, it would use less than 40% of this energy - about 4 watts a mile. \n Some calculations show that using land for solar panels to power e-bikes can get people more than 800x as far as using the same land to produce calories for the cyclist of a traditional manual bicycle (see Mike Berners-Lee's ❝There is no Planet B❞, 2019)." "bike"
        , EnergyUse "watching TV" Hour 50 "With an A++ TV consuming 50w an hour" "tv"
        , EnergyUse "of tea" CupOfTea 33 "Boiling only the water required, in a normal 3kW kettle. Kettles use about 6% of all the electricity in UK homes." "kettle"
        , EnergyUse "of popcorn" PopcornBag 27 "Assuming one bag takes 2 minutes in an 800w microwave" "microwave"
        , EnergyUse "cooking on an induction hob" Minute 33.333 "With a 2kW induction hob. Induction hobs have been found to use 60-40% less energy compared to a gas hob." "cooker"
        , EnergyUse "taking a shower" Minute 158.3333 "With a 9.5kW electric shower." "shower"
        , EnergyUse "vacuuming" Minute 15 "With a 900w compact vacuum cleaner" "vacuum"
        , EnergyUse "charging to full" IPhone6 22 "Newer larger iPhones require more energy to charge" "phone"
        , EnergyUse "using a laptop" Hour 58.4 "Based on an Apple MacBook Pro 15\" (2.9 GHz)" "laptop"
        , EnergyUse "of a washing machine on Baby Care (60ºC)" WashingCycle 2200 "The cycle lasts for 3 hours." "washing"
        , EnergyUse "powering a broadband router" Hour 6 "When you aren't using the internet, the idle consumption of a router might be as low as 2w, but under load, could be 20w ; 6w is used here as a rough average." "router"
        , EnergyUse "mined" Bitcoin 69710806.6971 "\"The Carbon Footprint of Bitcoin\" (2019) calculates that Bitcoin's annual electricity consumption is 45.8 TWh, and that 1,800 bitcoins are mined a day." "bitcoin"
        , EnergyUse "electricity in the average UK home" Hour 429.2237 "The average UK household uses about 3760 kWH annually." "house"
        , EnergyUse "electricity in the average US home" Hour 1404.109589 "The average US household uses about 12,300 kWH annually." "house"
        , EnergyUse "electricity in the average Chinese home" Hour 171.233 "The average Chinese household uses about 1,500 kWH annually." "house"
        , EnergyUse "of data downloaded through an ISP" Gigabyte 21.2 "Netflix, on a Medium quality setting, streams at `up to 0.7GB an hour`" "data"
        , EnergyUse "of a tumble dryer" WashingCycle 1670 "Based on getting 8kg of cotton `cupboard dry` in 130 minutes in an AEG Lavatherm A++ tumble dryer" "washing"
        ]
        1



-- UPDATE


type Msg
    = ChangeEnergyUse EnergyUse
    | ChangeQuantity Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeEnergyUse energyUse ->
            { model | selected = energyUse, quantity = 1 }

        ChangeQuantity quantity ->
            { model | quantity = quantity }


roundish : Float -> String
roundish n =
    if n < 0.9 then
        Round.round 0 (n * 100) ++ "%"
    else if (n < 2) && (n /= toFloat (Basics.round n)) then
        Round.round 0 n
    else
        n |> Round.round 0 |> commaize


worldCircumference =
    24901.55


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
            else if num >= (24 * 365) then
                Entry energyUse (num / (24 * 365)) "year"
            else if num >= (24 * 30) then
                Entry energyUse (num / (24 * 30)) "month"
            else if num >= (24 * 7) then
                Entry energyUse (num / (24 * 7)) "week"
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

        Gigabyte ->
            if num < (1 / 1024) then
                Entry energyUse (num * 1024 * 1024) "kilobyte"
            else if num < 1 then
                Entry energyUse (num * 1024) "megabyte"
            else if num > 1024 then
                Entry energyUse (num / 1024) "terrabyte"
            else
                Entry energyUse num (unitName energyUse.useUnit)

        {--
        Mile ->
            if num > worldCircumference then
                Entry energyUse (num / worldCircumference) "x around the world"
            else
                Entry energyUse num (unitName energyUse.useUnit)
                --}
        _ ->
            Entry energyUse num (unitName energyUse.useUnit)



-- VIEW


view : Model -> Html Msg
view model =
    div [ css [ width (Css.pct 90), margin auto, color (rgb 14 15 16), fontSize (pt 18), fontFamilies [ "Impact", "Trebuchet MS", "sans-serif" ] ] ]
        [ p
            []
            [ input
                [ css
                    [ width (Css.em 2)
                    , marginLeft (Css.em 0.5)
                    , margin (Css.em 0.5)
                    , padding (Css.em 0.125)
                    , fontSize (pt 38)
                    , fontWeight bold
                    , color (hex "#5779A8")
                    ]
                , type_ "number"
                , min "1"
                , value (String.fromInt model.quantity)
                , onInput
                    (\n -> ChangeQuantity (Maybe.withDefault 1 (String.toInt n)))
                ]
                []
            , text (pluralise (unitName model.selected.useUnit) model.quantity ++ " ")
            , strong [ css [ fontSize larger, fontWeight lighter, color (hex "#5779DD"), padding (Css.em 0.25) ] ] [ text model.selected.label ]
            , makeIcon model.selected.icon
            , text " uses the same energy as:"
            ]
        , div
            [ css
                [ fontStyle italic
                , float right
                , width (Css.em 20)
                , fontSize (Css.pt 14)
                , fontWeight lighter
                , fontFamilies [ "Arial", "Trebuchet MS", "sans-serif" ]
                , marginRight (Css.pct 15)
                , border3 (px 5) dotted (rgb 11 14 17)
                , padding (px 15)
                ]
            ]
            [ br [] []
            , h1
                [ css
                    [ color (hex "#149FEB")
                    , fontSize (Css.pt 70)
                    , fontStyle normal
                    , padding (Css.px 0)
                    , margin (Css.px 0)
                    , marginTop (Css.em -0.5)
                    , float left
                    ]
                ]
                [ text "ℹ"
                ]
            , span [] [ text model.selected.info ]
            ]
        , div []
            (model.energyUses
                |> List.filter (\eu -> eu /= model.selected)
                |> List.map (makeEntry (model.selected.wattHours * toFloat model.quantity))
                |> List.sortBy .num
                |> List.reverse
                |> List.map
                    (\entry ->
                        div [ class entry.energyUse.icon, css [] ]
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
                                [ onClick (ChangeEnergyUse entry.energyUse)
                                , href "#"
                                , css
                                    [ color (hex "#5779B7")
                                    , textDecoration none
                                    ]
                                ]
                                [ text (entry.energyUse.label ++ "  ")
                                , makeIcon entry.energyUse.icon
                                ]
                            ]
                    )
            )
        ]


makeIcon name =
    img
        [ src ("icons/" ++ name ++ ".png")
        , css
            [ marginBottom (Css.px -6)
            , width (Css.em 1.5)
            ]
        ]
        []


bigStrong txt =
    strong [ css [ fontSize (pt 38), color (hex "#5779A8"), padding (Css.em 0.25) ] ] [ text txt ]


pluralise name quantity =
    if quantity == 1 then
        name
    else
        name ++ "s"


is3x n =
    (n |> remainderBy 3) == 0 && n > 0


everyThird : Int -> String -> String
everyThird i a =
    if is3x i then
        a ++ ","
    else
        a


commaize : String -> String
commaize n =
    n
        |> String.split ""
        |> List.reverse
        |> List.indexedMap everyThird
        |> List.reverse
        |> String.join ""

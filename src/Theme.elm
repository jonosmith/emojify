module Theme
    exposing
        ( Size(Large, Larger, Largest, Medium, Regular, Small, Smaller, Smallest)
        , colorDanger
        , colorDangerLight
        , colorPrimary
        , colorPrimaryDarker
        , colorPrimaryDarkest
        , colorPrimaryLighter
        , colorSecondary
        , colorSecondaryLighter
        , colorSecondaryLightest
        , colorTransparent
        , fontSize
        , fonts
        )

import Color exposing (Color, rgb, rgba)


type Size
    = Smallest
    | Smaller
    | Small
    | Regular
    | Medium
    | Large
    | Larger
    | Largest


colorPrimary : Color
colorPrimary =
    rgb 34 184 207


colorPrimaryDarker : Color
colorPrimaryDarker =
    rgb 21 170 191


colorPrimaryDarkest : Color
colorPrimaryDarkest =
    rgb 16 152 173


colorPrimaryLighter : Color
colorPrimaryLighter =
    rgb 21 170 191


colorSecondary : Color
colorSecondary =
    rgb 51 58 64


colorSecondaryLighter : Color
colorSecondaryLighter =
    rgb 134 142 150


colorSecondaryLightest : Color
colorSecondaryLightest =
    rgb 233 236 239


colorDanger : Color
colorDanger =
    rgb 255 107 107


colorDangerLight : Color
colorDangerLight =
    rgba 255 107 107 0.2


colorTransparent : Color
colorTransparent =
    rgba 0 0 0 0.0


fonts : List String
fonts =
    [ "Roboto" ]


fontSize : Size -> Float
fontSize size =
    toFloat <|
        case size of
            Smallest ->
                10

            Smaller ->
                12

            Small ->
                14

            Regular ->
                16

            Medium ->
                18

            Large ->
                24

            Larger ->
                30

            Largest ->
                54

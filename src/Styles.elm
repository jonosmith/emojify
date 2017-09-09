module Styles
    exposing
        ( Styles
            ( Alert
            , Button
            , EditorContainer
            , EditorOverlay
            , ElementText
            , HomeDropzone
            , HomeDropzoneText
            , None
            , PageHeader
            , Slider
            , Textfield
            )
        , Variations
            ( Danger
            , Disabled
            , Hovering
            , PageHeaderLink
            , Primary
            , Secondary
            , Wide
            )
        , stylesheet
        )

{-| Application styles
-}

import Color exposing (Color, rgb, rgba)
import Color.Manipulate exposing (darken, fadeOut, lighten)
import Style exposing (StyleSheet, cursor, focus, hover, prop, pseudo, style, variation)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Alert
    | Button
    | EditorContainer
    | EditorOverlay
    | ElementText
    | HomeDropzone
    | HomeDropzoneText
    | PageHeader
    | Slider
    | Textfield


type Variations
    = Danger
    | Disabled
    | Hovering
    | PageHeaderLink
    | Primary
    | Secondary
    | Wide


type alias Config =
    { color :
        { primary : Color
        , secondary : Color
        , danger : Color
        , transparent : Color
        , white : Color
        }
    , font :
        { fonts : List String
        , size :
            { regular : Float
            , medium : Float
            , large : Float
            }
        }
    }


config : Config
config =
    { color =
        { primary = rgb 34 184 207
        , secondary = rgb 134 142 150
        , danger = rgb 255 107 107
        , transparent = rgba 0 0 0 0.0
        , white = Color.white
        }
    , font =
        { fonts = [ "Roboto" ]
        , size =
            { regular = 16.0
            , medium = 18.0
            , large = 54
            }
        }
    }


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style None []
        , style Alert
            [ Border.rounded 4.0
            , Font.size config.font.size.regular
            , Font.typeface config.font.fonts
            , variation Danger
                [ Color.text config.color.danger
                , Color.background (lighten 0.2 config.color.danger)
                ]
            ]
        , style Button
            [ Border.none
            , Border.rounded 4.0
            , Color.background config.color.transparent
            , Color.text config.color.primary
            , Font.size config.font.size.regular
            , Font.typeface config.font.fonts
            , cursor "pointer"
            , hover
                [ Color.background config.color.primary
                , Color.text config.color.white
                , Color.border config.color.transparent
                ]
            , variation Primary
                [ Color.background config.color.primary
                , Color.text config.color.white
                , hover
                    [ Color.background (darken 0.1 config.color.primary) ]
                , focus
                    [ Color.background (darken 0.2 config.color.primary)
                    ]
                ]
            , variation Secondary
                [ Color.background config.color.secondary
                , Color.text config.color.white
                , hover
                    [ Color.background (darken 0.1 config.color.secondary) ]
                , focus
                    [ Color.background (darken 0.2 config.color.secondary)
                    ]
                ]
            , variation Disabled
                [ Color.background Color.darkGray
                , Color.text config.color.white
                ]
            ]
        , style EditorContainer
            [ Border.all 2.0
            ]
        , style EditorOverlay
            [ Color.background config.color.transparent
            , hover
                [ cursor "move" ]
            ]
        , style ElementText
            [ Font.size config.font.size.regular
            , Font.typeface config.font.fonts
            ]
        , style HomeDropzone
            [ cursor "pointer"
            , variation Hovering
                [ Color.background (fadeOut 0.9 config.color.secondary)
                , Border.all 2.0
                , Border.dashed
                , Color.border config.color.primary
                ]
            ]
        , style HomeDropzoneText
            [ Font.size config.font.size.medium
            , Font.typeface config.font.fonts
            , Color.text (lighten 0.1 config.color.secondary)
            ]
        , style PageHeader
            [ Color.text config.color.primary
            , Font.typeface config.font.fonts
            , Font.size config.font.size.large
            , variation PageHeaderLink
                [ cursor "pointer" ]
            ]
        , style Slider
            [ prop "-webkit-appearance" "none"
            , pseudo ":-webkit-slider-thumb"
                [ prop "-webkit-appearance" "none"
                , prop "height" "38px"
                , prop "width" "10px"
                , prop "margin-top" "-16px"
                , Border.all 1.0
                , Border.rounded 2.0
                , Color.background config.color.white
                , Color.border config.color.secondary
                , Shadow.glow config.color.secondary 1.0
                ]
            , pseudo ":-ms-track"
                [ Color.background config.color.transparent
                , Color.border config.color.transparent
                , Color.text config.color.transparent
                ]
            , pseudo ":-webkit-slider-runnable-track"
                [ prop "height" "4px"
                , Color.background config.color.secondary
                ]
            ]
        , style Textfield
            [ Border.bottom 1
            , Color.border (Color.rgba 0 0 0 0.12)
            , Color.text config.color.secondary
            , Font.typeface config.font.fonts
            , Font.size config.font.size.medium
            ]
        ]

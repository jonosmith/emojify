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
            , Wide
            )
        , stylesheet
        )

{-| Application styles
-}

import Color exposing (Color, rgb, rgba)
import Style exposing (StyleSheet, cursor, focus, hover, style, variation)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


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
    | Wide


type alias Config =
    { color :
        { primary : Color
        , primaryDarker : Color
        , primaryDarkest : Color
        , secondary : Color
        , secondaryLighter : Color
        , secondaryLightest : Color
        , danger : Color
        , dangerLight : Color
        , transparent : Color
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
        , primaryDarker = rgb 21 170 191
        , primaryDarkest = rgb 16 152 173
        , secondary = rgb 51 58 64
        , secondaryLighter = rgb 134 142 150
        , secondaryLightest = rgb 233 236 239
        , danger = rgb 255 107 107
        , dangerLight = rgba 255 107 107 0.2
        , transparent = rgba 0 0 0 0.0
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
            [ Font.size config.font.size.regular
            , Font.typeface config.font.fonts
            , Border.rounded 4.0
            , variation Danger
                [ Color.text config.color.danger
                , Color.background config.color.dangerLight
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
                , Color.text Color.white
                , Color.border config.color.transparent
                ]
            , variation Primary
                [ Color.background config.color.primary
                , Color.text Color.white
                , hover
                    [ Color.background config.color.primaryDarker ]
                , focus
                    [ Color.background config.color.primaryDarkest
                    ]
                ]
            , variation Disabled
                [ Color.background Color.darkGray
                , Color.text Color.white
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
                [ Color.background config.color.secondaryLightest
                , Border.all 2.0
                , Border.dashed
                , Color.border config.color.primary
                ]
            ]
        , style HomeDropzoneText
            [ Font.size config.font.size.medium
            , Font.typeface config.font.fonts
            , Color.text config.color.secondaryLighter
            ]
        , style PageHeader
            [ Color.text config.color.primary
            , Font.typeface config.font.fonts
            , Font.size config.font.size.large
            , variation PageHeaderLink
                [ cursor "pointer" ]
            ]
        , style Slider
            []
        , style Textfield
            [ Border.bottom 1
            , Color.border (Color.rgba 0 0 0 0.12)
            , Font.typeface config.font.fonts
            , Font.size config.font.size.medium
            , Color.text config.color.secondary
            ]
        ]

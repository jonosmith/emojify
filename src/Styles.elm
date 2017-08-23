module Styles exposing (Styles(..), Variations(..), stylesheet)

import Color exposing (rgb, rgba)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Theme


type Styles
    = None
    | Alert
    | ElementText
    | Button
    | Textfield
    | Slider
    | HomeDropzone
    | HomeDropzoneText
    | PageHeader
    | EditorContainer
    | EditorOverlay


type Variations
    = ButtonPrimary
    | ButtonWide
    | ButtonDisabled
    | Danger
    | HomeDropzoneHovering
    | PageHeaderLink


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style None []
        , style ElementText
            [ Font.size (Theme.fontSize Theme.Regular)
            , Font.typeface Theme.fonts
            ]
        , style Alert
            [ Font.size (Theme.fontSize Theme.Regular)
            , Font.typeface Theme.fonts
            , Border.rounded 4.0
            , variation Danger
                [ Color.text Theme.colorDanger
                , Color.background Theme.colorDangerLight
                ]
            ]
        , style Button
            [ Border.none
            , Border.rounded 4.0
            , Color.background Theme.colorTransparent
            , Color.text Theme.colorPrimary
            , Font.size (Theme.fontSize Theme.Regular)
            , Font.typeface Theme.fonts
            , cursor "pointer"
            , hover
                [ Color.background Theme.colorPrimary
                , Color.text Color.white
                , Color.border Theme.colorTransparent
                ]
            , variation ButtonPrimary
                [ Color.background Theme.colorPrimary
                , Color.text Color.white
                , hover
                    [ Color.background Theme.colorPrimaryDarker ]
                , focus
                    [ Color.background Theme.colorPrimaryDarkest
                    ]
                ]
            , variation ButtonDisabled
                [ Color.background Color.darkGray
                , Color.text Color.white
                ]
            ]
        , style Textfield
            [ Border.bottom 1
            , Color.border (Color.rgba 0 0 0 0.12)
            , Font.typeface Theme.fonts
            , Font.size (Theme.fontSize Theme.Medium)
            , Color.text Theme.colorSecondary
            ]
        , style Slider
            []
        , style HomeDropzone
            [ cursor "pointer"
            , variation HomeDropzoneHovering
                [ Color.background Theme.colorSecondaryLightest
                , Border.all 2.0
                , Border.dashed
                , Color.border Theme.colorPrimary
                ]
            ]
        , style HomeDropzoneText
            [ Font.size (Theme.fontSize Theme.Medium)
            , Font.typeface Theme.fonts
            , Color.text Theme.colorSecondaryLighter
            ]
        , style PageHeader
            [ Color.text Theme.colorPrimary
            , Font.typeface Theme.fonts
            , Font.size (Theme.fontSize Theme.Largest)
            , variation PageHeaderLink
                [ cursor "pointer" ]
            ]
        , style EditorContainer
            [ Border.all 2.0
            ]
        , style EditorOverlay
            [ Color.background Theme.colorTransparent
            , hover
                [ cursor "move" ]
            ]
        ]

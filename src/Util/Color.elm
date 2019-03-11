module Util.Color exposing (..)

import Basics.Extra as Basics
import Css
import Color


rotateBy : Float -> Color.Color -> Color.Color
rotateBy turns color =
    let
        ({hue} as hsla) = Color.toHsla color
        nextHue = hue + turns |> Basics.fractionalModBy 1
    in
        Color.fromHsla { hsla | hue = nextHue }

toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        {red, green, blue, alpha} = Color.toRgba color
    in
        Css.rgba
            (round <| red * 255)
            (round <| green * 255)
            (round <| blue * 255)
            alpha

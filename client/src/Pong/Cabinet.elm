module Pong.Cabinet exposing
    ( logo
    , viewHole
    )

-- IMPORTS

import Svg
import Svg.Attributes



-- VIEW


logo : Svg.Svg msg
logo =
    Svg.svg
        [ Svg.Attributes.version "1.0"
        , Svg.Attributes.width "800"
        , Svg.Attributes.height "238"
        , Svg.Attributes.viewBox "0 0 800 238"
        , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "translate(0,238) scale(0.05,-0.05)"
            , Svg.Attributes.fill "black"
            ]
            [ Svg.path [ Svg.Attributes.d "M2785 2319 c-356 -51 -684 -291 -840 -613 -206 -425 -123 -920 210 -1251 239 -238 582 -357 937 -325 298 26 520 124 715 313 184 179 285 388 313 652 23 222 -18 443 -118 639 -60 116 -114 187 -221 288 -172 161 -400 265 -651 298 -90 11 -262 11 -345 -1z m390 -439 c240 -75 419 -250 501 -490 27 -78 27 -282 0 -360 -38 -112 -88 -192 -171 -275 -84 -84 -154 -131 -255 -172 -399 -161 -844 61 -957 477 -28 103 -23 264 10 365 78 236 274 414 520 470 89 21 262 13 352 -15z" ] []
            , Svg.path [ Svg.Attributes.d "M5108 2320 c-288 -46 -512 -238 -578 -496 -19 -74 -20 -114 -20 -891 l0 -813 220 0 220 0 2 803 3 802 22 41 c50 94 146 153 262 162 137 9 247 -38 306 -133 l30 -48 5 -811 5 -811 220 0 220 0 0 810 0 810 -23 75 c-71 229 -266 404 -534 480 -84 24 -269 34 -360 20z" ] []
            , Svg.path [ Svg.Attributes.d "M7370 2304 c-153 -26 -235 -52 -355 -112 -129 -64 -218 -125 -313 -216 -238 -228 -344 -497 -329 -836 15 -335 180 -647 450 -851 141 -106 318 -182 489 -209 107 -17 379 -8 478 17 l75 18 3 553 2 552 -330 0 -330 0 0 -200 0 -200 100 0 100 0 0 -166 0 -167 -51 7 c-189 25 -394 209 -486 436 -74 183 -65 348 26 535 81 165 226 292 401 351 161 54 340 54 487 -1 36 -14 69 -25 74 -25 5 0 9 104 9 230 l0 230 -27 11 c-73 28 -159 40 -298 44 -82 2 -161 2 -175 -1z" ] []
            , Svg.path [ Svg.Attributes.d "M122 1203 l3 -1088 215 0 215 0 3 406 2 406 173 6 c195 6 251 17 372 76 176 87 310 249 360 437 25 92 30 235 12 332 -36 188 -156 346 -327 429 -154 75 -155 76 -618 80 l-412 5 2 -1089z m783 686 c55 -15 137 -100 158 -164 46 -134 -14 -285 -143 -362 l-54 -33 -148 0 -148 0 0 278 c0 153 3 282 7 285 11 11 285 8 328 -4z" ] []
            ]
        ]


viewHole : Svg.Svg msg
viewHole =
    Svg.svg
        [ Svg.Attributes.version "1.0"
        , Svg.Attributes.width "1000"
        , Svg.Attributes.height "800"
        , Svg.Attributes.viewBox "0 0 1000 800"
        , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
        ]
        [ Svg.rect
            [ Svg.Attributes.style "clip-path: polygon(0 0, 100% 0, 96% 100%, 4% 100%);"
            , Svg.Attributes.fill "black"
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            , Svg.Attributes.width "1000"
            , Svg.Attributes.height "800"
            ]
            []
        ]

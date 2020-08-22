module Util.Icon exposing
    ( dev
    , github
    , home
    , thumbsUp
    , twitter
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- ICONS


dev : Svg a
dev =
    Svg.svg
        [ Svg.Attributes.class "h-6 mr-2 w-6"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.viewBox "0 0 512 512"
        , Svg.Attributes.width "512"
        , Svg.Attributes.height "512"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M0,91.4v329.1h512V91.4H0z M36.6,128h438.9v256H36.6V128z M73.1,164.6v182.9H128c30.2,0,54.9-24.6,54.9-54.9v-73.1\n\t\tc0-30.2-24.6-54.9-54.9-54.9H73.1z M256,164.6c-20.2,0-36.6,16.4-36.6,36.6v109.7c0,20.2,16.4,36.6,36.6,36.6h36.6v-36.6H256v-36.6\n\t\th36.6v-36.6H256v-36.6h36.6v-36.6H256z M323.5,164.6l38.1,165c2.4,10.4,11.7,17.8,22.4,17.8s20-7.4,22.4-17.8l38.1-165H407L384,264\n\t\tl-23-99.5H323.5z M109.7,201.1H128c10.1,0,18.3,8.2,18.3,18.3v73.1c0,10.1-8.2,18.3-18.3,18.3h-18.3V201.1z" ]
            []
        ]


github : Svg a
github =
    Svg.svg
        [ Svg.Attributes.class "h-6 mr-2 w-6"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.viewBox "0 0 440 440"
        , Svg.Attributes.width "440"
        , Svg.Attributes.height "440"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M409.132,114.573c-19.608-33.596-46.205-60.194-79.798-79.8C295.736,15.166,259.057,5.365,219.271,5.365\n\t\tc-39.781,0-76.472,9.804-110.063,29.408c-33.596,19.605-60.192,46.204-79.8,79.8C9.803,148.168,0,184.854,0,224.63\n\t\tc0,47.78,13.94,90.745,41.827,128.906c27.884,38.164,63.906,64.572,108.063,79.227c5.14,0.954,8.945,0.283,11.419-1.996\n\t\tc2.475-2.282,3.711-5.14,3.711-8.562c0-0.571-0.049-5.708-0.144-15.417c-0.098-9.709-0.144-18.179-0.144-25.406l-6.567,1.136\n\t\tc-4.187,0.767-9.469,1.092-15.846,1c-6.374-0.089-12.991-0.757-19.842-1.999c-6.854-1.231-13.229-4.086-19.13-8.559\n\t\tc-5.898-4.473-10.085-10.328-12.56-17.556l-2.855-6.57c-1.903-4.374-4.899-9.233-8.992-14.559\n\t\tc-4.093-5.331-8.232-8.945-12.419-10.848l-1.999-1.431c-1.332-0.951-2.568-2.098-3.711-3.429c-1.142-1.331-1.997-2.663-2.568-3.997\n\t\tc-0.572-1.335-0.098-2.43,1.427-3.289c1.525-0.859,4.281-1.276,8.28-1.276l5.708,0.853c3.807,0.763,8.516,3.042,14.133,6.851\n\t\tc5.614,3.806,10.229,8.754,13.846,14.842c4.38,7.806,9.657,13.754,15.846,17.847c6.184,4.093,12.419,6.136,18.699,6.136\n\t\tc6.28,0,11.704-0.476,16.274-1.423c4.565-0.952,8.848-2.383,12.847-4.285c1.713-12.758,6.377-22.559,13.988-29.41\n\t\tc-10.848-1.14-20.601-2.857-29.264-5.14c-8.658-2.286-17.605-5.996-26.835-11.14c-9.235-5.137-16.896-11.516-22.985-19.126\n\t\tc-6.09-7.614-11.088-17.61-14.987-29.979c-3.901-12.374-5.852-26.648-5.852-42.826c0-23.035,7.52-42.637,22.557-58.817\n\t\tc-7.044-17.318-6.379-36.732,1.997-58.24c5.52-1.715,13.706-0.428,24.554,3.853c10.85,4.283,18.794,7.952,23.84,10.994\n\t\tc5.046,3.041,9.089,5.618,12.135,7.708c17.705-4.947,35.976-7.421,54.818-7.421s37.117,2.474,54.823,7.421l10.849-6.849\n\t\tc7.419-4.57,16.18-8.758,26.262-12.565c10.088-3.805,17.802-4.853,23.134-3.138c8.562,21.509,9.325,40.922,2.279,58.24\n\t\tc15.036,16.18,22.559,35.787,22.559,58.817c0,16.178-1.958,30.497-5.853,42.966c-3.9,12.471-8.941,22.457-15.125,29.979\n\t\tc-6.191,7.521-13.901,13.85-23.131,18.986c-9.232,5.14-18.182,8.85-26.84,11.136c-8.662,2.286-18.415,4.004-29.263,5.146\n\t\tc9.894,8.562,14.842,22.077,14.842,40.539v60.237c0,3.422,1.19,6.279,3.572,8.562c2.379,2.279,6.136,2.95,11.276,1.995\n\t\tc44.163-14.653,80.185-41.062,108.068-79.226c27.88-38.161,41.825-81.126,41.825-128.906\n\t\tC438.536,184.851,428.728,148.168,409.132,114.573z" ]
            []
        ]


home : Svg a
home =
    Svg.svg
        [ Svg.Attributes.class "h-6 mr-2 w-6"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.viewBox "0 0 20 20"
        , Svg.Attributes.width "20"
        , Svg.Attributes.height "20"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M10.707 2.293a1 1 0 00-1.414 0l-7 7a1 1 0 001.414 1.414L4 10.414V17a1 1 0 001 1h2a1 1 0 001-1v-2a1 1 0 011-1h2a1 1 0 011 1v2a1 1 0 001 1h2a1 1 0 001-1v-6.586l.293.293a1 1 0 001.414-1.414l-7-7z" ]
            []
        ]


thumbsUp : Svg a
thumbsUp =
    Svg.svg
        [ Svg.Attributes.class "h-6 mr-2 w-6"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width "24"
        , Svg.Attributes.height "24"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M2 10.5a1.5 1.5 0 113 0v6a1.5 1.5 0 01-3 0v-6zM6 10.333v5.43a2 2 0 001.106 1.79l.05.025A4 4 0 008.943 18h5.416a2 2 0 001.962-1.608l1.2-6A2 2 0 0015.56 8H12V4a2 2 0 00-2-2 1 1 0 00-1 1v.667a4 4 0 01-.8 2.4L6.8 7.933a4 4 0 00-.8 2.4z" ]
            []
        ]


twitter : Svg a
twitter =
    Svg.svg
        [ Svg.Attributes.class "h-6 mr-2 w-6"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width "24"
        , Svg.Attributes.height "24"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M24 4.557c-.883.392-1.832.656-2.828.775 1.017-.609 1.798-1.574 2.165-2.724-.951.564-2.005.974-3.127 1.195-.897-.957-2.178-1.555-3.594-1.555-3.179 0-5.515 2.966-4.797 6.045-4.091-.205-7.719-2.165-10.148-5.144-1.29 2.213-.669 5.108 1.523 6.574-.806-.026-1.566-.247-2.229-.616-.054 2.281 1.581 4.415 3.949 4.89-.693.188-1.452.232-2.224.084.626 1.956 2.444 3.379 4.6 3.419-2.07 1.623-4.678 2.348-7.29 2.04 2.179 1.397 4.768 2.212 7.548 2.212 9.142 0 14.307-7.721 13.995-14.646.962-.695 1.797-1.562 2.457-2.549z" ]
            []
        ]

module Gauss exposing (gauss, gaussJordan)

{-| The Gauss and Gauss-Jordan Elimination Algorithms, on type `Matrix`
from the [`eeue56/elm-flat-matrix` package](http://package.elm-lang.org/packages/eeue56/elm-flat-matrix/latest),
over `Rational`s from the [`imeckler/ratio` package](http://package.elm-lang.org/packages/imeckler/ratio/latest).

@docs gauss, gaussJordan
-}

import Matrix exposing (Matrix)
import Ratio exposing (Rational)
import Array
import List.Extra as List


{-| `gauss 3` on matrix

    [ [ 3, 4,  0, 2 ]
    , [ 1, 0, -3, 1 ]
    , [ 0, 2, -1, 5 ]
    ]

gives matrix

    [ [ 1, 4/3,   0,  2/3 ]
    , [ 0,   1, 9/4, -1/4 ]
    , [ 0,   0,   1,   -1 ]
    ]
-}
gauss : Int -> Matrix Rational -> Matrix Rational
gauss n =
    let
        swapIfNecessary i j m =
            if i == j then
                m
            else
                List.foldl (\c -> Matrix.set c j (safeGet c i m) >> Matrix.set c i (safeGet c j m)) m (List.range i n)

        forward i m =
            case Matrix.getColumn i m of
                Nothing ->
                    Debug.crash "IMPOSSIBLE!"

                Just column ->
                    case List.find (\( _, v ) -> Ratio.numerator v /= 0) (List.drop i (Array.toIndexedList column)) of
                        Nothing ->
                            m

                        Just ( j, v ) ->
                            let
                                m_ =
                                    swapIfNecessary i j m
                                        |> Matrix.set i i one
                                        |> flip (List.foldl (\c -> Matrix.update c i (flip Ratio.divide v))) (List.range (i + 1) n)
                            in
                                List.foldl
                                    (\r ->
                                        let
                                            f =
                                                Ratio.negate (safeGet i r m_)
                                        in
                                            Matrix.set i r zero
                                                >> flip (List.foldl (\c -> Matrix.update c r (Ratio.add (Ratio.multiply f (safeGet c i m_))))) (List.range (i + 1) n)
                                    )
                                    m_
                                    (List.range (i + 1) (n - 1))
    in
        flip (List.foldl forward) (List.range 0 (n - 1))


{-| `gaussJordan 3` on matrix

    [ [ 3, 4,  0, 2 ]
    , [ 1, 0, -3, 1 ]
    , [ 0, 2, -1, 5 ]
    ]

gives matrix

    [ [ 1, 0, 0, -2 ]
    , [ 0, 1, 0,  2 ]
    , [ 0, 0, 1, -1 ]
    ]
-}
gaussJordan : Int -> Matrix Rational -> Matrix Rational
gaussJordan n =
    let
        backward i m =
            if Ratio.numerator (safeGet i i m) == 0 then
                m
            else
                List.foldl
                    (\r ->
                        let
                            k =
                                Ratio.negate (safeGet i r m)
                        in
                            Matrix.set i r zero
                                >> flip (List.foldl (\c -> Matrix.update c r (Ratio.add (Ratio.multiply k (safeGet c i m))))) (List.range (i + 1) n)
                    )
                    m
                    (List.range 0 (i - 1))
    in
        gauss n >> flip (List.foldl backward) (List.reverse (List.range 1 (n - 1)))


safeGet i j m =
    case Matrix.get i j m of
        Nothing ->
            Debug.crash "IMPOSSIBLE!"

        Just x ->
            x


zero =
    Ratio.fromInt 0


one =
    Ratio.fromInt 1

module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Pendulums" (400,400) (10,10)


-- Best bits to mess with
background :: Color
-- background = makeColour 43 45 66 255
background = makeColour 30 30 46 255

rainCols :: [Color]
rainCols = [ makeColour 251 248 204 255
           , makeColour 253 228 207 255
           , makeColour 255 207 210 255
           , makeColour 241 192 232 255
           , makeColour 207 186 240 255
           , makeColour 163 196 243 255
           , makeColour 144 219 244 255
           , makeColour 142 236 245 255
           , makeColour 152 245 225 255
           , makeColour 185 251 192 255
           ]

rainGrad :: Float -> Float -> Color
rainGrad x len = mixColors ((len/9)-remainder) remainder (rainCols !! multiples) (rainCols !! (multiples +1))
    where multiples :: Int
          multiples = floor (x / (len/9))
          remainder = x - fromIntegral multiples * (len/9)

g :: Float
g = 9.8

initPendulum :: Penduli
initPendulum = Penduli
    { r1 = 200
    , m1 = 20
    , a1 = pi/19
    , a1' = 0
    , a1'' = 0

    , r2 = 200
    , m2 = 200
    , a2 = 0
    , a2' = 0
    , a2'' = 0

    , colour = white
    }

numPendulums :: Float
numPendulums = 2000

gap :: Float
gap = 0.00005
--

makeColour :: Float -> Float -> Float -> Float -> Color
makeColour r gr b a = makeColor (r/255) (gr/255) (b/255) (a/255)

fps :: Int
fps = 30

toDeg :: Floating a => a -> a
toDeg = (180*) . (/pi)

sine :: Floating a => a -> a
sine = sin . toDeg

cosine :: Floating a => a -> a
cosine = cos . toDeg

data Penduli = Penduli
    { r1 :: Float
    , m1 :: Float
    , a1 :: Float
    , a1' :: Float
    , a1'' :: Float

    , r2 :: Float
    , m2 :: Float
    , a2 :: Float
    , a2' :: Float
    , a2'' :: Float

    , colour :: Color
    }


newtype Model = Model
    { penduli :: [Penduli]    }


initialState :: Model
initialState = Model 
    {penduli = map (\x -> initPendulum {
        a1 = a1 initPendulum + gap*x
--        , colour = mixColors x (numPendulums-x) grad1 grad2
        , colour = rainGrad x numPendulums
        }) [1..numPendulums-1]
    }

render :: Model -> Picture
render model = translate 0 0 $ pictures $ map renderDouble (penduli model)

renderDouble :: Penduli -> Picture
renderDouble pend = color (colour pend) $ pictures
                [ renderPend (m1 pend) (0,0) (x1,y1)
                , renderPend (m2 pend) (x1,y1) (x2,y2)] 
    where x1 = r1 pend * sine (a1 pend)
          y1 = negate $ r1 pend * cosine (a1 pend)
          x2 = x1 + r2 pend * sine (a2 pend)
          y2 = (y1+) $ negate $ r2 pend * cosine (a2 pend)


renderPend :: Float -> Point -> Point -> Picture
renderPend _ p1 p2 = pictures [ line [p1, p2]
                                 , uncurry translate p2 $ circleSolid (20/4)] 



updatePenduli :: ViewPort -> Float -> Model -> Model
updatePenduli _ t model = Model { penduli = map (updatePos t . updateVel t . updateAcc) (penduli model) }

updateAcc :: Penduli -> Penduli
updateAcc pend = pend 
    { a1'' = updatePend1 pend
    , a2'' = updatePend2 pend
    }

updateVel :: Float -> Penduli -> Penduli
updateVel t pend = pend 
    { a1' = 0.999 * ( a1' pend + t * a1'' pend)
    , a2' = 0.999 * (a2' pend + t * a2'' pend)
    }

updatePos :: Float -> Penduli -> Penduli
updatePos t pend = pend 
    { a1 = a1 pend + t * a1' pend
    , a2 = a2 pend + t * a2' pend 
    }




updatePend1 :: Penduli -> Float
updatePend1 pend = ( num1 - num2 - num3Coeff * num3 ) / num4
    where gV = g
          m1V = m1 pend
          r1V = r1 pend
          a1V = a1 pend
          a1'V = a1' pend
          m2V = m2 pend
          r2V = r2 pend
          a2V = a2 pend
          a2'V = a2' pend

          num1 =  (-gV) * (2*m1V + m2V) * sine a1V 
          num2 = m2V * gV * sine (a1V - 2*a2V)
          num3Coeff = 2*sine (a1V - a2V) * m2V 
          num3 = a2'V * a2'V * r2V + a1'V * a1'V * r1V * cosine (a1V-a2V)
          num4 = r1V * (2*m1V + m2V - m2V * cosine (2*a1V - 2*a2V))



updatePend2 :: Penduli -> Float
updatePend2 pend =  numCoeff * ( num1 + num2 + num3) / den
    where gV = g
          m1V = m1 pend
          r1V = r1 pend
          a1V = a1 pend
          a1'V = a1' pend
          m2V = m2 pend
          r2V = r2 pend
          a2V = a2 pend
          a2'V = a2' pend

          numCoeff = 2*sine (a1V - a2V) 
          num1 = a1'V * a1'V * r1V * (m1V + m2V)
          num2 = gV * (m1V + m2V) * cosine a1V
          num3 = a2'V * a2'V * r2V * m2V * cosine (a1V - a2V)
          den = r1V * (2*m1V + m2V - m2V * cosine (2*a1V - 2*a2V))
 
main :: IO ()
main = simulate window background fps initialState render updatePenduli

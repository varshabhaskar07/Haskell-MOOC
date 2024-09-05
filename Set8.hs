module Set8 where

import Data.Char (intToDigit)
import Mooc.Todo
import Codec.Picture

-- Defining the Color data type with three Int fields for red, green, and blue.
data Color = Color Int Int Int
  deriving (Show, Eq)

-- Extracting the red component from a Color.
getRed :: Color -> Int
getRed (Color r _ _) = r

-- Extracting the green component from a Color.
getGreen :: Color -> Int
getGreen (Color _ g _) = g

-- Extracting the blue component from a Color.
getBlue :: Color -> Int
getBlue (Color _ _ b) = b

-- Defining some predefined colors.
black :: Color
black = Color 0 0 0

white :: Color
white = Color 255 255 255

pink :: Color
pink = Color 255 105 180

red :: Color
red = Color 255 0 0

yellow :: Color
yellow = Color 255 240 0

-- Defining the Coord data type for coordinates.
data Coord = Coord Int Int

-- Defining the Picture data type as a function from Coord to Color.
data Picture = Picture (Coord -> Color)

-- Creating a simple picture with a white dot at (10, 10) and black elsewhere.
justADot :: Picture
justADot = Picture f
  where f (Coord 10 10) = white
        f _             = black

-- Creating a solid color picture.
solid :: Color -> Picture
solid color = Picture (\coord -> color)

-- Creating an example picture with different colors based on coordinate values.
examplePicture1 :: Picture
examplePicture1 = Picture f
  where f (Coord x y) | abs (x+y) < 100 = pink    -- Top-left area pink.
                      | max x y < 200 = white     -- White square surrounding the pink area.
                      | otherwise = black         -- Black elsewhere.

-- Rendering a Picture to a PNG file.
render :: Picture -> Int -> Int -> String -> IO ()
render (Picture f) w h name = writePng name (generateImage (\x y -> colorToPixel (f (Coord x y))) w h)
  where colorToPixel :: Color -> PixelRGB8
        colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- Converting an Int to a hexadecimal string.
showHex :: Int -> String
showHex i = [digit (div i 16), digit (mod i 16)]
  where digit x | x >= 0 && x < 16 = intToDigit x
                | otherwise         = 'X'

-- Converting a Color to a hexadecimal string.
colorToHex :: Color -> String
colorToHex (Color r g b) = showHex r ++ showHex g ++ showHex b

-- Getting the pixel color at (x, y) in a Picture as a hexadecimal string.
getPixel :: Picture -> Int -> Int -> String
getPixel (Picture f) x y = colorToHex (f (Coord x y))

-- Rendering a Picture to a list of lists of hexadecimal strings.
renderList :: Picture -> (Int, Int) -> (Int, Int) -> [[String]]
renderList picture (minx, maxx) (miny, maxy) =
  [[getPixel picture x y | x <- [minx..maxx]] | y <- [miny..maxy]]

-- Example usage of renderList with justADot.
renderListExample :: [[String]]
renderListExample = renderList justADot (9,11) (9,11)

------------------------------------------------------------------------------
-- Ex 1

-- Creating a picture with a white dot at (3, 4), a pink line at y=8, and black elsewhere.
dotAndLine :: Picture
dotAndLine = Picture f
  where f (Coord 3 4) = white  -- White dot at (3, 4).
        f (Coord _ 8) = pink   -- Pink line at y=8.
        f _           = black  -- Black elsewhere.

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 2

-- Blending two colors by averaging their components.
blendColor :: Color -> Color -> Color
blendColor (Color r1 g1 b1) (Color r2 g2 b2) =
  Color (div (r1 + r2) 2) (div (g1 + g2) 2) (div (b1 + b2) 2)

-- Combining two pictures with a given color blending function.
combine :: (Color -> Color -> Color) -> Picture -> Picture -> Picture
combine f (Picture p1) (Picture p2) = Picture (\coord -> f (p1 coord) (p2 coord))

-- Blending two pictures by blending their colors.
blend :: Picture -> Picture -> Picture
blend = combine blendColor

-- Defining the Shape data type as a function from Coord to Bool.
data Shape = Shape (Coord -> Bool)

-- Checking if a Shape contains a point (x, y).
contains :: Shape -> Int -> Int -> Bool
contains (Shape f) x y = f (Coord x y)

-- Creating a dot shape at (x, y).
dot :: Int -> Int -> Shape
dot x y = Shape f
  where f (Coord cx cy) = (x == cx) && (y == cy)

-- Creating a circular shape with radius r and center (cx, cy).
circle :: Int -> Int -> Int -> Shape
circle r cx cy = Shape f
  where f (Coord x y) = (x - cx)^2 + (y - cy)^2 < r^2

-- Filling a shape with a color to create a picture.
fill :: Color -> Shape -> Picture
fill c (Shape f) = Picture g
  where g coord | f coord = c
                | otherwise = black

-- Creating an example picture with a red circle.
exampleCircle :: Picture
exampleCircle = fill red (circle 80 100 200)

------------------------------------------------------------------------------
-- Ex 3

-- Creating a rectangular shape with top-left corner (x0, y0) and dimensions w x h.
rectangle :: Int -> Int -> Int -> Int -> Shape
rectangle x0 y0 w h = Shape f
  where f (Coord x y) = x0 <= x && x < x0 + w && y0 <= y && y < y0 + h

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 4

-- Combining two shapes with a union operation.
union :: Shape -> Shape -> Shape
union (Shape f1) (Shape f2) = Shape (\coord -> f1 coord || f2 coord)

-- Cutting out one shape from another.
cut :: Shape -> Shape -> Shape
cut (Shape f1) (Shape f2) = Shape (\coord -> f1 coord && not (f2 coord))

------------------------------------------------------------------------------

-- Creating an example snowman picture.
exampleSnowman :: Picture
exampleSnowman = fill white snowman
  where snowman = union (cut body mouth) hat
        mouth = rectangle 180 180 40 5
        body = union (circle 50 200 250) (circle 40 200 170)
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

------------------------------------------------------------------------------
-- Ex 5

-- Painting a shape with a solid color on top of an existing picture.
paintSolid :: Color -> Shape -> Picture -> Picture
paintSolid color (Shape f1) (Picture g1) = Picture g
  where g c = if f1 c then color else g1 c

-- Creating a solid white picture.
allWhite :: Picture
allWhite = solid white

-- Creating an example colorful picture with multiple shapes painted in different colors.
exampleColorful :: Picture
exampleColorful = (paintSolid black hat . paintSolid red legs . paintSolid pink body) allWhite
  where legs = circle 50 200 250   -- Defining the 'legs' shape as a circle
        body = circle 40 200 170   -- Defining the 'body' shape as a circle
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)  -- Combining rectangles to form a hat

-- Creating a stipple pattern picture with two colors.
stipple :: Color -> Color -> Picture
stipple a b = Picture f
  where f (Coord x y) | even x == even y = a
                      | otherwise         = b

-- Creating a stripes pattern picture with two colors.
stripes :: Color -> Color -> Picture
stripes a b = Picture f
  where f (Coord x y) | even y = a
                      | otherwise = b

------------------------------------------------------------------------------

-- Ex 6

-- Defining the 'paint' function, taking a picture, a shape, and another picture
-- Returning a new picture based on the shape's condition
paint :: Picture -> Shape -> Picture -> Picture
paint (Picture g1) (Shape f1) (Picture g2) = Picture g
  where g c = if f1 c then g1 c else g2 c

-- Creating examplePatterns by sequentially painting different patterns over a white background
examplePatterns :: Picture
examplePatterns = (paint (solid black) hat . paint (stripes red yellow) legs . paint (stipple pink black) body) allWhite
  where legs = circle 50 200 250      -- Defining the 'legs' shape as a circle
        body = circle 40 200 170      -- Defining the 'body' shape as a circle
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)  -- Combining rectangles to form a hat

-- Flipping coordinates for XY
flipCoordXY :: Coord -> Coord
flipCoordXY (Coord x y) = (Coord y x)

-- Flipping a picture along the XY axis
flipXY :: Picture -> Picture
flipXY (Picture f) = Picture (f . flipCoordXY)

-- Zooming in on coordinates by a factor
zoomCoord :: Int -> Coord -> Coord
zoomCoord z (Coord x y) = Coord (div x z) (div y z)

-- Applying zoom to a picture
zoom :: Int -> Picture -> Picture
zoom z (Picture f) = Picture (f . zoomCoord z)

-- Creating large vertical stripes by zooming and flipping stripes
largeVerticalStripes :: Picture
largeVerticalStripes = zoom 5 (flipXY (stripes red yellow))

-- Defining the Transform class for applying transformations to pictures
class Transform t where
  apply :: t -> Picture -> Picture

-- Defining a simple XY color pattern
xy :: Picture
xy = Picture f
  where f (Coord x y) = Color (mod x 256) (mod y 256) 0

------------------------------------------------------------------------------

-- Ex 7

-- Defining the Fill data type for filling a picture with a single color
data Fill = Fill Color

-- Implementing the Transform instance for Fill
instance Transform Fill where
  apply (Fill color) _ = Picture (\c -> color)

-- Defining the Zoom data type for zoom transformations
data Zoom = Zoom Int
  deriving Show

-- Implementing the Transform instance for Zoom
instance Transform Zoom where
  apply (Zoom m) p = zoom m p

-- Defining the Flip data type for various flip transformations
data Flip = FlipX | FlipY | FlipXY
  deriving Show

-- Implementing the Transform instance for Flip
instance Transform Flip where
  apply FlipX (Picture f) = Picture (\(Coord x y) -> f (Coord (-x) y))  -- Flipping horizontally
  apply FlipY (Picture f) = Picture (\(Coord x y) -> f (Coord x (-y)))  -- Flipping vertically
  apply FlipXY p          = flipXY p  -- Flipping both horizontally and vertically

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 8

-- Defining the Chain data type for chaining transformations
data Chain a b = Chain a b
  deriving Show

-- Implementing the Transform instance for chaining transformations
instance (Transform a, Transform b) => Transform (Chain a b) where
  apply (Chain a b) p = (apply a . apply b) p

-- Creating large vertical stripes by chaining zoom and flip transformations
largeVerticalStripes2 :: Picture
largeVerticalStripes2 = apply (Chain (Zoom 5) FlipXY) (stripes red yellow)

-- Defining a function to blend a picture with its flipped version
flipBlend :: Picture -> Picture
flipBlend picture = blend picture (apply FlipXY picture)

-- Creating a checkered pattern by blending vertical stripes with their flipped version
checkered :: Picture
checkered = flipBlend largeVerticalStripes2

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 9

-- Defining the Blur data type for blur transformations
data Blur = Blur
  deriving Show

-- Implementing the Transform instance for Blur
instance Transform Blur where
  apply _ (Picture f) = Picture (\(Coord x y) -> avg (map (f . co) $ neighbours x y))
    where co t = Coord (fst t) (snd t)
          neighbours x y = [(x, y), (x-1, y), (x+1, y), (x, y-1), (x, y+1)]  -- Defining neighbouring coordinates
          avg xs = divColor (foldr sumColor (Color 0 0 0) xs) (length xs)  -- Calculating average color

-- Summing colors for blur effect
sumColor :: Color -> Color -> Color
sumColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

-- Dividing color by an integer for averaging
divColor :: Color -> Int -> Color
divColor (Color x y z) i = Color (div x i) (div y i) (div z i)

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Ex 10

-- Defining the BlurMany data type for multiple blur transformations
data BlurMany = BlurMany Int
  deriving Show

-- Implementing the Transform instance for multiple blur transformations
instance Transform BlurMany where
  apply (BlurMany n) = foldr (.) id (replicate n (apply Blur))

-- Applying multiple blurs to create a blurred snowman
blurredSnowman :: Picture
blurredSnowman = apply (BlurMany 2) exampleSnowman

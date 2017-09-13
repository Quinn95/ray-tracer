import Data.Maybe (isNothing, isJust, fromJust)
import Data.Vector.Class
import Data.Vector.V3
import Control.Applicative ((<$>), (<*>))
import Debug.Trace (trace)

type Vec3 = Vector3
type Point = Vector3
type Color = Vector3
type Radius = Double 
type Distance = Double
type Direction = Vec3

-- origin, direction (
data Ray = Ray Point Direction deriving (Show)

direction (Ray _ d) = d

makeRay :: Point -> Direction -> Ray
makeRay o d = Ray o (vnormalise d)

data Material = Material {color :: Color
                         ,specular :: Double
                         ,lambert :: Double
                         ,ambient :: Double
                         } deriving (Show)


data Object = Sphere Point Radius Material
--            | Cube Point Double Material
              deriving (Show)

            -- | Plane Int Int Material


material :: Object -> Material
material (Sphere _ _ x) = x
--material (Cube _ _ _ x) = x


surface_normal :: Point -> Object -> Vec3
surface_normal p (Sphere o _ _) = vnormalise $ p - o


data Scene = Scene { camera :: Point
                   , objects :: [Object]
                   , lights :: [Point]
                   , width :: Double
                   , height :: Double 
                   }

test_obj = Sphere (Vector3 5.0 5.0 5.0) 10.0 (Material (Vector3 255.0 0.0 0.0) 0.0 0.0 0.0)


--a = putStrLn . show $ get_intersection test_scene test_ray

get_intersection :: Scene -> Ray -> Maybe (Object, Distance)
get_intersection s r = foldr (\obj closest -> min' obj (intersect r obj) closest) Nothing (objects s)
    where
        min' obj Nothing Nothing = Nothing
        min' obj (Just mdist) Nothing = Just (obj, mdist)
        min' obj Nothing x = x
        min' obj (Just mdist) cobj@(Just (_, closest)) 
          | (mdist < closest) == True = Just (obj, mdist)
          | otherwise = cobj 


a = makeRay (Vector3 0.0 0.0 0.0) (Vector3 (0.0) 0.0 0.0)
b = Sphere (Vector3 10.0 0.0 0.0) 1.0 (Material (Vector3 0.0 0.0 0.0) 0.0 0.0 0.0)
c = Scene {camera = Vector3 0.0 0.0 0.0
                   ,objects = [b]
                   ,lights = []
                   ,width = 10.0
                   ,height = 10.0
                   }



intersect :: Ray -> Object -> Maybe Distance 
intersect (Ray ro d) (Sphere so r _)
  | discriminant >= 0 && dist > 0 = Just dist
  | otherwise = Nothing
    where
        sphere_to_ray = ro - so
        b = 2 * (vdot d sphere_to_ray)
        c = (vdot sphere_to_ray sphere_to_ray) - r ** 2.0
        discriminant = b ** 2  - 4 * c
        dist = (-b - (sqrt discriminant)) / 2


point_at_dist :: Ray -> Distance -> Point
point_at_dist (Ray o dir) dist = o + dir |* dist


black :: Color
black = Vector3 0.0 0.0 0.0


vector_reflect v1 v2 = v1 - (2 * (vdot v1 (vnormalise v2))) *| v2

tracer :: Scene -> Ray -> Int -> Color
tracer s r depth
  | depth > 4 || isNothing inter = black 
  | otherwise = lighting_function + (tracer s reflective_ray (depth+1)) |* (obj_specular)
     where
         inter = get_intersection s r  
         obj = fst . fromJust $ inter
         dist = snd . fromJust $ inter
         inter_point = point_at_dist r dist
         surface_norm = surface_normal inter_point obj
         obj_color = color . material $ obj
         obj_ambient = ambient . material $ obj
         obj_specular = specular . material $ obj
         obj_lambert = lambert . material $ obj
         obj_ambient_color = obj_color |* obj_ambient
         reflective_ray = makeRay inter_point (vnormalise (vector_reflect (direction r) surface_norm))
         lighting_function = foldr (\l color -> ((obj_lambert * (lightf l)) *| (obj_color)) + color) obj_ambient_color (lights s)
             where 
                 lightf :: Point -> Double
                 lightf l 
                   | (isNothing maybe_intersection) && lamb_intensity > 0 = lamb_intensity
                   | otherwise = 0
                    where
                        lamb_intensity = vdot surface_norm pt_to_light_vec 
                        pt_to_light_vec = vnormalise $ l - inter_point
                        pt_to_light_ray = makeRay (inter_point) pt_to_light_vec
                        maybe_intersection = get_intersection s pt_to_light_ray
                        

type Pixels = [[Color]]
render :: Scene -> Pixels
render scene = foldr (\y rows -> render' y : rows) [] [0.0..(height scene)]
    where
        render' y = foldr (\x columns -> tracer scene (ray x y) 0 : columns) [] [0.0..(width scene)]
            where
                ray x y = let cam = camera scene in makeRay cam ((Vector3 x y 0.0) - cam)




print_color :: Color -> String
print_color (Vector3 x y z) = (f x) ++ " " ++ (f y) ++ " " ++ (f z) ++ " "
    where
        f a = show . floor $ a

pixels_to_ppm :: [[Vec3]] -> String
pixels_to_ppm px = header ++ test
    where 
        test = foldr (\y rows -> (rows ++ (foldr (\x clmns -> clmns ++ print_color x) "" y) ++ "\n")) "" px
        header = "P3\n" ++ (show (length (px !! 1))) ++ " " ++ (show (length px)) ++ " 255\n"


makeSphere :: Double -> Double -> Double -> Double -> Material -> Object
makeSphere x y z r m = Sphere (Vector3 x y z) r m


main = do
    let mat1 = Material {color = (Vector3 0xff 0.0 0.0), specular = 0.2, lambert = 1.0, ambient = 0.2} 
        mat2 = Material {color = (Vector3 0.0 0.0 0xff), specular = 0.8, lambert = 1.0, ambient = 0.5}
        mat3 = Material {color = (Vector3 0.0 0xff 0.0), specular = 0.5, lambert = 1.0, ambient = 0.2}
        mat4 = Material {color = (Vector3 0xff 0xff 0.0), specular = 0.8, lambert = 1.0, ambient = 0.2}
        mat5 = Material {color = (Vector3 0xff 0xff 0xff), specular = 0.8, lambert = 0.5, ambient = 0.2}
        ob1 = makeSphere 150.0 120.0 (-20.0) 80.0 mat1
        ob2 = makeSphere 420.0 120.0 0.0 100.0 mat2
        ob3 = makeSphere 320.0 240.0 (-40.0) 50.0 mat3
        ob4 = makeSphere 300.0 200.0 (200.0) 100.0 mat4
        ob5 = makeSphere 300.0 1000.0 (0.0) 700.0 mat5
        test_scene = Scene {camera = Vector3 (200.0) 200.0 (-400.0)
                           ,objects = [ob1, ob2, ob3, ob4, ob5]
                           ,lights = [(Vector3 200.0 (-100.0) 0), (Vector3 600 200 (-200))]
                           ,width = 640.0
                           ,height = 480.0
                           }
    putStrLn . pixels_to_ppm . render $ test_scene
    --putStrLn . show .  render $ test_scene
    {-let a = render test_scene
        b = pixels_to_ppm a
    putStr "Length: "
    putStrLn . show . length $ a !! 1
    putStrLn . show . length $ b
-}












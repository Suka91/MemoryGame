import Prelude
import Data.List.Split
import Data.Foldable hiding (foldl)
import Graphics.UI.SDL as SDL
import System.Posix.Unistd

initParamsFile = "resources/init_board.txt"
resourceLocationsFile = "resources/locations.txt"
background = "resources/background.bmp"

main = do 	
		initParamsCoordsIO <- readFile initParamsFile
		initParamsLocationsIO <- readFile resourceLocationsFile
   		let initParamsCoords = map (\(x,y) -> (read x :: Int, read y  :: Int,0,0)) $ parseRawInput (words initParamsCoordsIO)
   		let initParamsLocations = map (\(x,y) -> (read x :: Int, y)) $ parseRawInput (words initParamsLocationsIO)
		let initState = zipWith (\(a,b,c,d) (e,f) -> (a,b,c,d,e,f)) initParamsCoords initParamsLocations

		SDL.init [InitEverything]
		setVideoMode 590 475 32 []
		screen <- getVideoSurface
		back <- loadBMP background
		startGame screen initState back

startGame sc gameState clearScreen = do
										render sc gameState clearScreen
										SDL.flip sc					    

render sc gameState clearScreen = do
									blitSurface clearScreen Nothing sc Nothing
									forM_ gameState $ \(a,b,c,d,e,f) -> do
										pic <- loadBMP f
										blitSurface pic Nothing sc (Just (Rect a b 0 0))

parseRawInput inputString = foldl (\acc x -> acc ++ [(head $ (splitOn "," x),head $ tail $ (splitOn "," x))]) [] (splitOn ";" (head inputString))

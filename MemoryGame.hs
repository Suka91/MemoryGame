import Prelude hiding (catch)
import Data.List (sortBy)
import Data.List.Split
import Data.Foldable hiding (foldl)
import Graphics.UI.SDL as SDL
import System.Posix.Unistd
import Control.Exception
import System.Random

initParamsFile = "resources/init_board.txt"
resourceLocationsFile = "resources/locations.txt"
background = "resources/background.bmp"
backTheme = "resources/backTheme.bmp"

main = do 	
		gen <- getStdGen 
		let randomNumbers = makeRandomGens 20 gen
		initParamsCoordsIO <- readFile initParamsFile
		initParamsLocationsIO <- readFile resourceLocationsFile
   		let initParamsCoords = map (\(x,y) -> (read x :: Int, read y  :: Int,0,0)) $ parseRawInput (words initParamsCoordsIO)
   		let initParamsLocations = map (\(x,y) -> y) $ sortBy randomCompare $ zip randomNumbers $ map (\(x,y) -> (read x :: Int, y)) $ parseRawInput (words initParamsLocationsIO)

		let initState = zipWith (\(a,b,c,d) (e,f) -> (a,b,c,d,e,f)) initParamsCoords initParamsLocations

		
		SDL.init [InitEverything]
		setVideoMode 590 475 32 []
		startGame initState

makeRandomGens 0 _ = []
makeRandomGens n gen = let (value, newGen) = random gen :: (Int,StdGen)
		       in value:makeRandomGens (n - 1) newGen

randomCompare (a1,b1) (a2,b2)
     | a1 < a2     = GT  
     | a2 == a1    = EQ  
     | otherwise = LT

startGame gameState = do
			render gameState
			loop gameState 0		

handleEvent (MouseButtonDown mx my ButtonLeft) gameState = do
								render newGameState
								return newGameState
							where newGameState =  handleMouseClick x y gameState
							      x = fromIntegral mx
							      y = fromIntegral my

loop gameState turn = do
	if(turn == 2) then do
		newGameState <- handleSecondTurn gameState
		loop newGameState 0
	else do
		event <- waitEvent
		case event of 
			MouseButtonDown _ _ _ -> do
				newGameState <- handleEvent event gameState
				loop newGameState $ calculateTurn newGameState
			_ -> loop gameState turn

handleSecondTurn gameState = return $ map (\(a,b,c,d,e,f) -> if ((c == 1) && (checkForPair e gameState)) then (a,b,c,1,e,f) else (a,b,0,d,e,f)) gameState

calculateTurn gameState = foldl (\acc (a,b,c,d,e,f) -> if ((c == 1) && (d == 0)) then acc + 1 else acc) 0 gameState

checkForPair id gameState = if (2 == foldl (\acc (a,b,c,d,e,f) -> if ((id == e) && (c == 1)) then (acc + 1) else acc) 0 gameState) then True else False

handleMouseClick x y gameState = map (\(a,b,c,d,e,f) -> if((x > a) && (x < a + 100) && (y > b) && (y < b + 100) && (c == 0) && (d == 0))
						then (a,b,1,d,e,f)
						else (a,b,c,d,e,f)) gameState
render gameState = do
			screen <- getVideoSurface
			clearScreen <- loadBMP background
			blitSurface clearScreen Nothing screen Nothing
			backThemeLoaded <- loadBMP backTheme
			forM_ gameState $ \(a,b,c,d,e,f) -> do
				if (c == 1) 
				then do
					pic <- loadBMP f
					blitSurface pic Nothing screen (Just (Rect a b 0 0))
				else do
					blitSurface backThemeLoaded Nothing screen (Just (Rect a b 0 0))
			SDL.flip screen

parseRawInput inputString = foldl (\acc x -> acc ++ [(head $ (splitOn "," x),head $ tail $ (splitOn "," x))]) [] (splitOn ";" (head inputString))

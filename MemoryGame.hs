import Prelude hiding (catch)
import Data.List (sortBy)
import Data.List.Split
import Data.Foldable hiding (foldl)
import Graphics.UI.SDL as SDL
import System.Posix.Unistd
import Control.Exception
import System.Random
import qualified Graphics.UI.SDL.TTF as TTF
import System.Time
import Data.Time.Clock

initParamsFile = "resources/init_board.txt"
resourceLocationsFile = "resources/locations.txt"
background = "resources/background.bmp"
backTheme = "resources/backTheme.bmp"
buttonBackground = "resources/buttonBackground.bmp"
scoreFile = "resources/scores.csv"


data MainMenuClick = DUMMY_CLICK | PLAY_GAME_CLICK | SCORE_CLICK | BACK_TO_GAME_CLICK | EXIT_CLICK	deriving (Enum,Eq)

--font for text
text :: String
text = "./resources/text.ttf"

--font for logo
logo :: String
logo = "./resources/logo.ttf"

main = do 

		SDL.init [InitEverything]
		TTF.init
		goToMainMenu

goToMainMenu = do
				buttonPressedIO <- mainMenuScreen
				buttonPressed <- buttonPressedIO
				if (buttonPressed == PLAY_GAME_CLICK)
					then do
						a <- gameScreen
						print a
						scoreScreen
					else if (buttonPressed == SCORE_CLICK)
					then scoreScreen
					else return 0	

scoreScreen = do
	renderScoreScreen
	scoreScreenLoop

scoreScreenLoop = do
		    event <- waitEvent
		    case event of 
		    	MouseButtonDown _ _ _ -> do
				scoreEvenet <- handleEventScoreScreen event
				if(scoreEvenet == DUMMY_CLICK)
					then scoreScreenLoop
					else if(scoreEvenet == BACK_TO_GAME_CLICK)
						then do
							goToMainMenu
						else scoreScreenLoop
			_ -> scoreScreenLoop

handleEventScoreScreen (MouseButtonDown mx my ButtonLeft) = do	
																if((x > 220) && (x < 370) && (y > 370) && (y < 420))
																	then return BACK_TO_GAME_CLICK
																	else return DUMMY_CLICK 
																where x = fromIntegral mx
														      		      y = fromIntegral my
   
renderScoreScreen = do
	screen <- getVideoSurface
	clearScreen <- loadBMP background
	buttonMainMenu <- loadBMP buttonBackground 	
	blitSurface clearScreen Nothing screen Nothing
	blitSurface buttonMainMenu Nothing screen (Just(Rect 220 360 0 0))
	
	font <- TTF.openFont text 50
	textSurface <- TTF.renderUTF8Solid font "SCORE" (SDL.Color 255 255 255)
	blitSurface textSurface Nothing screen (Just(Rect 230 50 0 0))

	font <- TTF.openFont text 35
	textSurface <- TTF.renderUTF8Solid font "Main menu" (SDL.Color 0 0 0)
	blitSurface textSurface Nothing screen (Just(Rect 220 370 0 0))

--ceo score
	font <- TTF.openFont text 25 
	scoreInfoString <- scoreInfo
	
	let yCoordinates = [150, 175, 200, 225, 250]
	let scoreCoordinates = zipWith (\(a,b) (c) -> (a,b,c)) scoreInfoString yCoordinates
	forM_ scoreCoordinates $ \(a,b,c) -> do
		textSurface <- TTF.renderUTF8Solid font (a++ "  ...........  " ++ b) (SDL.Color 255 255 255)
		blitSurface textSurface Nothing screen(Just(Rect 190 c 0 0))
	
	SDL.flip screen

-- f-ja koja vadi 5 najboljih rezultata
scoreInfo = do 
	content <- readFile scoreFile
	let scoreList = parseScoreInput $ words content
	return $ firstFive scoreList
	--let scoreList = map (\(x,y) -> (read x :: String, read y  :: String)) $ parseRawInput (words content)
	--firstFive <- return (show (take 5 $ sortBy scoreCompare scoreList))

firstFive scoreList =
	take 5 $ sortBy scoreCompare scoreList
        

scoreCompare (a1,b1) (a2,b2)
     | b1 > b2     = GT  
     | b2 == b1    = EQ  
     | otherwise = LT
		
--function for main menu screen
mainMenuScreen = do
	setVideoMode 590 475 32 []
	renderMainMenuScreen
	return $ loopMainMenu
	
loopMainMenu = do
		event <- waitEvent
		case event of 
			MouseButtonDown _ _ _ -> do
				mainMenuEvent <- handleEventMainMenu event
				if(mainMenuEvent == DUMMY_CLICK)
					then loopMainMenu
					else if(mainMenuEvent == PLAY_GAME_CLICK)
						then return PLAY_GAME_CLICK
						else if(mainMenuEvent == EXIT_CLICK)
						then return EXIT_CLICK
						else return SCORE_CLICK	
			_ -> loopMainMenu

handleEventMainMenu (MouseButtonDown mx my ButtonLeft) = do	
							if((x > 220) && (x < 370) && (y > 270) && (y < 320))
								then return PLAY_GAME_CLICK
								else if((x > 220) && (x < 370) && (y > 340) && (y < 390))
									then return SCORE_CLICK
									else if ((x > 220) && (x < 370) && (y > 410) && (y < 450))
										then return EXIT_CLICK
										else return DUMMY_CLICK 
							where x = fromIntegral mx
					      		      y = fromIntegral my
								
renderMainMenuScreen = do
	screen <- getVideoSurface
	clearScreen <- loadBMP background
	blitSurface clearScreen Nothing screen Nothing
	buttonMainMenu <- loadBMP buttonBackground 
	blitSurface buttonMainMenu Nothing screen (Just(Rect 220 270 0 0))
	blitSurface buttonMainMenu Nothing screen (Just(Rect 220 340 0 0))
	blitSurface buttonMainMenu Nothing screen (Just(Rect 220 410 0 0))
	
	font <- TTF.openFont text 35
	textSurface <- TTF.renderUTF8Solid font "PLAY" (SDL.Color 0 0 0)
	blitSurface textSurface Nothing screen (Just(Rect 260 280 0 0))

	font <- TTF.openFont text 35
	textSurface <- TTF.renderUTF8Solid font "SCORE" (SDL.Color 0 0 0)
	blitSurface textSurface Nothing screen (Just(Rect 250 350 0 0))

	font <- TTF.openFont text 35
	textSurface <- TTF.renderUTF8Solid font "EXIT" (SDL.Color 0 0 0)
	blitSurface textSurface Nothing screen (Just(Rect 270 420 0 0))

	font <- TTF.openFont logo 130
	textSurface <- TTF.renderUTF8Solid font "MEMORY GAME" (SDL.Color 255 255 255)
	blitSurface textSurface Nothing screen (Just(Rect 45 100 0 0))

	SDL.flip screen
		
--function for game screen
gameScreen = do

	beginTime <- getCurrentTime
	gen <- getStdGen 
	let randomNumbers = makeRandomGens 20 gen
	initParamsCoordsIO <- readFile initParamsFile
	initParamsLocationsIO <- readFile resourceLocationsFile
	let initParamsCoords = map (\(x,y) -> (read x :: Int, read y  :: Int,0,0)) $ parseRawInput (words initParamsCoordsIO)
	let initParamsLocations = map (\(x,y) -> y) $ sortBy randomCompare $ zip randomNumbers $ map (\(x,y) -> (read x :: Int, y)) $ parseRawInput (words initParamsLocationsIO)

	--(x,y,flip,pogodjena,id,lokacija)
	let initState = zipWith (\(a,b,c,d) (e,f) -> (a,b,c,d,e,f)) initParamsCoords initParamsLocations

	startGame initState beginTime


makeRandomGens 0 _ = []
makeRandomGens n gen = let (value, newGen) = random gen :: (Int,StdGen)
		       in value:makeRandomGens (n - 1) newGen

randomCompare (a1,b1) (a2,b2)
     | a1 < a2     = GT  
     | a2 == a1    = EQ  
     | otherwise = LT

startGame gameState beginTime = do
			render gameState
			loop gameState 0 beginTime		


handleEvent (MouseButtonDown mx my ButtonLeft) gameState = do
								render newGameState
								return newGameState
							where newGameState =  handleMouseClick x y gameState
							      x = fromIntegral mx
							      y = fromIntegral my

loop gameState turn beginTime = do
	if(isGameFinished gameState == True) then do
		endTime <- getCurrentTime
		appendFile scoreFile (scoreLine "ime" beginTime endTime)
		
	else if(turn == 2) then do
		newGameState <- handleSecondTurn gameState
		loop newGameState 0 beginTime
	else do
		event <- waitEvent
		case event of 
			MouseButtonDown _ _ _ -> do
				newGameState <- handleEvent event gameState
				loop newGameState (calculateTurn newGameState) beginTime 
			_ -> loop gameState turn beginTime

--pravljenje linije za upis u scoreFile
scoreLine name beginTime endTime = 
	let nameSpace = ";" ++ name ++ ","
	    nameTime = nameSpace ++ show (diffUTCTime endTime beginTime)
	in nameTime

isGameFinished gameState = foldl (\acc (a,b,c,d,e,f) -> if (d == 0) then False else acc) True gameState

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
--ovo nam je dorada, pokreni i vidi zasto u skoru postoje torka u torki
--zasto se posle odredjenog vremena ne moze da otvori text.tff
--ne gasi komp, da bi sutra sa posla mogli da upadnemo opet kod tebe
parseScoreInput inputString = foldl (\acc x -> acc ++ [(head $ (splitOn "," x), last $ (splitOn "," x))]) [] (splitOn ";" (head inputString))

{-# LANGUAGE ParallelListComp #-}

import World
import Body
import Util
import Timing
import MainArgs
import System.Environment
import Points2D.Generate
import Graphics.Gloss
import System.Console.ParseArgs
import Control.Monad
import qualified Data.Vector.Unboxed		as V
import qualified Solver.Naive.Solver		as Naive
import qualified Solver.List.Draw		as BHL
import qualified Solver.List.Solver		as BHL
import qualified Solver.Vector.Solver		as BHV


main :: IO ()
main  
 = do	args	<- parseArgsIO ArgsComplete mainArgs
	
	when (gotArg args ArgHelp)
	 $ usageError args ""

	mainWithArgs args
	

mainWithArgs :: Args MainArg -> IO ()
mainWithArgs args
 = do	let Just windowSize	= getArgInt	args ArgWindowSize
	let shouldDrawTree	= gotArg  	args ArgDrawTree
	let Just timeWarp	= getArgDouble	args ArgTimeWarp
	let Just bodyCount	= getArgInt	args ArgBodyCount
	let Just bodyMass	= getArgDouble  args ArgBodyMass
	let Just epsilon	= getArgDouble	args ArgEpsilon
	let Just discSize	= getArgDouble	args ArgDiscSize
	let Just startSpeed	= getArgDouble	args ArgStartSpeed
	
	let vPoints 	= genPointsDisc bodyCount (0, 0) discSize

	let vBodies	= V.map (setStartVelOfBody startSpeed)
			$ V.map (setMassOfBody     bodyMass)
			$ V.map (uncurry unitBody) 
			$ vPoints

	let advance	= advanceWorld (calcAccels_naive epsilon) (10 * timeWarp) 

	simulateInWindow
		"Barnes-Hutt"			-- window name
		(windowSize, windowSize)	-- window size
		(10, 10)			-- window position
		black				-- background color
		30				-- number of iterations per second
		vBodies				-- initial world
		(drawWorld shouldDrawTree)	-- fn to convert a world to a picture
		advance				-- fn to advance the world


calcAccels_naive :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_naive epsilon
	= Naive.calcAccels epsilon 
	
calcAccels_bhList :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_bhList epsilon mpts
	= V.fromList
	$ BHL.calcAccels epsilon
	$ V.toList mpts

calcAccels_bhVector :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_bhVector epsilon mpts
	= V.fromList
	$ BHV.calcAccels epsilon
	$ V.toList mpts




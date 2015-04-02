{-# LANGUAGE TemplateHaskell #-}
module Main where


import Criterion.Main
import Language.Haskell.TH
import Examples
import Deconstructor
import qualified Query as HDB



main = defaultMain [
		bgroup "Range LINQ" [ bench "Range 30 40" $ whnfIO $ fromTest [|$(range) 30 40|],
						 bench "Range 0 100" $ whnfIO $ fromTest [|$(range) 0 100|],
						 bench "Range 0 10" $ whnfIO $ fromTest [|$(range) 0 10|]
						],
		bgroup "Range TLINQ" [ bench "Range 30 40" $ whnfIO $ fromTestTyped [||$$(rangeT) 30 40||],
						 bench "Range 0 100" $ whnfIO $ fromTestTyped [||$$(rangeT) 0 100||],
						 bench "Range 0 10" $ whnfIO $ fromTestTyped [||$$(rangeT) 0 10||]
						],
		bgroup "RangeHDB" [ bench "RangeHDB 30 40" $ whnfIO $ (HDB.range 30 40),
						 bench "RangeHDB 0 100" $ whnfIO $ (HDB.range 0 100),
						 bench "RangeHDB 0 10" $ whnfIO $ (HDB.range 0 10)
						],
		bgroup "Differences" [ bench "LINQ" $ whnfIO $ fromTest [|$(differences)|],
							   bench "TLINQ" $ whnfIO $ fromTestTyped [||$$(differencesT)||],
							   bench "HaskellDB" $ whnfIO $ HDB.differences
							 ],
	    bgroup "GetAge" [ bench "LINQ Drew" $ whnfIO $ fromTest [|$(getAge) "Drew"|],
	    				  bench "LINQ unknown name " $ whnfIO $ fromTest [|$(getAge) "Ethan"|],
	    				  bench "TLINQ Drew" $ whnfIO $ fromTestTyped [||$$(getAgeT) "Drew"||],
	    				  bench "TLINQ unknown name " $ whnfIO $ fromTestTyped [||$$(getAgeT) "Ethan"||], 
	    				  bench "HaskellDB Drew" $ whnfIO $ (HDB.getAge "Drew"), 
	    				  bench "HaskellDB unknown name " $ whnfIO $ (HDB.getAge "Ethan")
	    				],
	    bgroup "Compose" [ bench "LINQ Edna Drew" $ whnfIO $ fromTest [|$(compose) "Edna" "Drew"|],
	    				   bench "LINQ John Tom" $ whnfIO $ fromTest [|$(compose) "Jon" "Tom"|],
	    				   bench "TLINQ Edna Drew" $ whnfIO $ fromTestTyped [||$$(composeT) "Edna" "Drew"||],
	    				   bench "TLINQ John Tom" $ whnfIO $ fromTestTyped [||$$(composeT) "Jon" "Tom"||],
	    				   bench "HaskellDB Edna Drew" $ whnfIO $ HDB.compose "Edna" "Drew",
	    				   bench "HaskellDB John Tom" $ whnfIO $ HDB.compose "Jon" "Tom",
	    				   bench "HaskellDB' Edna Drew" $ whnfIO $ HDB.compose' "Edna" "Drew",
	    				   bench "HaskellDB' John Tom" $ whnfIO $ HDB.compose' "Jon" "Tom"
	    				 ],
	    bgroup "Satisfies" [ bench "LINQ" $ whnfIO $ fromTest [|$satisfies $pre|],
	    					 bench "TLINQ" $ whnfIO $ fromTestTyped [||$$satisfiesT $$preT||]
	    				   ],

	    bgroup "Satisfies Dynamic" [ bench "TLINQ" $ whnfIO $ fromTestTyped [||$$satisfiesT $$(p t0)||]
	    						   ],
	    bgroup "Normalisation" [ bench "range" $ whnf  normalise [|$range 30 40|],
	    						 bench "differences" $ whnf  normalise [|$differences|],
	    						 bench "getAge" $ whnf  normalise [|$getAge "Drew"|],
	    						 bench "compose" $ whnf  normalise [|$compose "Edna" "Drew"|],
	    						 bench "satisfies" $ whnf normalise [|$satisfies $pre|]
	    					   ]
	]

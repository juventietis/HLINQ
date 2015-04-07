{-# LANGUAGE TemplateHaskell #-}
module Main where


import Criterion.Main
import Language.Haskell.TH
import Examples
import Deconstructor
import qualified Query as HDB



main = defaultMain [
		bgroup "Range LINQ" [ bench "Range 30 40" $ whnfIO $ fromTestUntyped [|$(range) 30 40|],
						 bench "Range 0 100" $ whnfIO $ fromTestUntyped [|$(range) 0 100|],
						 bench "Range 0 10" $ whnfIO $ fromTestUntyped [|$(range) 0 10|]
						],
		bgroup "Range TLINQ" [ bench "Range 30 40" $ whnfIO $ fromTest [||$$(rangeT) 30 40||],
						 bench "Range 0 100" $ whnfIO $ fromTest [||$$(rangeT) 0 100||],
						 bench "Range 0 10" $ whnfIO $ fromTest [||$$(rangeT) 0 10||]
						],
		bgroup "RangeHDB" [ bench "RangeHDB 30 40" $ whnfIO $ (HDB.range 30 40),
						 bench "RangeHDB 0 100" $ whnfIO $ (HDB.range 0 100),
						 bench "RangeHDB 0 10" $ whnfIO $ (HDB.range 0 10)
						],
		bgroup "Differences" [ bench "LINQ" $ whnfIO $ fromTestUntyped [|$(differences)|],
							   bench "TLINQ" $ whnfIO $ fromTest [||$$(differencesT)||],
							   bench "HaskellDB" $ whnfIO $ HDB.differences
							 ],
	    bgroup "GetAge" [ bench "LINQ Drew" $ whnfIO $ fromTestUntyped [|$(getAge) "Drew"|],
	    				  bench "LINQ unknown name " $ whnfIO $ fromTestUntyped [|$(getAge) "Ethan"|],
	    				  bench "TLINQ Drew" $ whnfIO $ fromTest [||$$(getAgeT) "Drew"||],
	    				  bench "TLINQ unknown name " $ whnfIO $ fromTest [||$$(getAgeT) "Ethan"||], 
	    				  bench "HaskellDB Drew" $ whnfIO $ (HDB.getAge "Drew"), 
	    				  bench "HaskellDB unknown name " $ whnfIO $ (HDB.getAge "Ethan")
	    				],
	    bgroup "Compose" [ bench "LINQ Edna Drew" $ whnfIO $ fromTestUntyped [|$(compose) "Edna" "Drew"|],
	    				   bench "LINQ John Tom" $ whnfIO $ fromTestUntyped [|$(compose) "Jon" "Tom"|],
	    				   bench "TLINQ Edna Drew" $ whnfIO $ fromTest [||$$(composeT) "Edna" "Drew"||],
	    				   bench "TLINQ John Tom" $ whnfIO $ fromTest [||$$(composeT) "Jon" "Tom"||],
	    				   bench "HaskellDB Edna Drew" $ whnfIO $ HDB.compose "Edna" "Drew",
	    				   bench "HaskellDB John Tom" $ whnfIO $ HDB.compose "Jon" "Tom",
	    				   bench "HaskellDB' Edna Drew" $ whnfIO $ HDB.compose' "Edna" "Drew",
	    				   bench "HaskellDB' John Tom" $ whnfIO $ HDB.compose' "Jon" "Tom"
	    				 ],
	    bgroup "Satisfies" [ bench "LINQ" $ whnfIO $ fromTestUntyped [|$satisfies $pre|],
	    					 bench "TLINQ" $ whnfIO $ fromTest [||$$satisfiesT $$preT||]
	    				   ],

	    bgroup "Satisfies Dynamic" [ bench "TLINQ" $ whnfIO $ fromTest [||$$satisfiesT $$(p t0)||]
	    						   ],
	    bgroup "Normalisation" [ bench "range" $ whnf  normalise [|$range 30 40|],
	    						 bench "differences" $ whnf  normalise [|$differences|],
	    						 bench "getAge" $ whnf  normalise [|$getAge "Drew"|],
	    						 bench "compose" $ whnf  normalise [|$compose "Edna" "Drew"|],
	    						 bench "satisfies" $ whnf normalise [|$satisfies $pre|]
	    					   ]
	]


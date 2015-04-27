{-# LANGUAGE TemplateHaskell #-}
module Main where


import Criterion.Main
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Examples
import Database.HLINQ.Deconstructor
import Database.HLINQ.Utilities
import System.IO.Unsafe
import qualified Query as HDB

normalisedCompose fun = do
	exp <- runQ $ (normalise $ unTypeQ [||$$composeT "Edna" "Drew"||])
	fun exp 
normalisedDifferences fun = do
	exp <- runQ $ (normalise $ unTypeQ [||$$differencesT||])
	fun exp

printNormalised query = do
	exp <- runQ $ (normalise $ unTypeQ query)
	print exp

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

		bgroup "Range" [ bench "Range 30 40" $ whnfIO $ fromTest [||$$(rangeT) 30 40||],
						 bench "Range 0 100" $ whnfIO $ fromTest [||$$(rangeT) 0 100||],
						 bench "Range 0 10" $ whnfIO $ fromTest [||$$(rangeT) 0 10||],
						 bench "HLINQ 30 40" $ nfIO $ ((toList $ fromTest [||$$(rangeT) 30 40||]):: IO [(String)]),
						 bench "HLINQ 0 100" $ nfIO $ ((toList $ fromTest [||$$(rangeT) 0 100||]):: IO [(String)]),
						 bench "HLINQ 0 10" $ nfIO $ ((toList $ fromTest [||$$(rangeT) 0 10||]):: IO [(String)]),
						 bench "RangeHDB 30 40" $ whnfIO $ (HDB.range 30 40),
						 bench "RangeHDB 0 100" $ whnfIO $ (HDB.range 0 100),
						 bench "RangeHDB 0 10" $ whnfIO $ (HDB.range 0 10)
				   		],
		bgroup "Differences" [ bench "HLINQ" $ whnfIO $ fromTest [||$$(differencesT)||],
							   bench "HLINQ to Tuples" $ nfIO $ ((toTuple toTup2 $ fromTest [||$$differencesT||]):: IO [(String, Int)]),
							   bench "HaskellDB" $ nfIO $ HDB.differences
							 ],

	    bgroup "GetAge" [ bench "HLINQ Drew" $ whnfIO $ fromTest [||$$(getAgeT) "Drew"||],
	    				  bench "HLINQ Drew to list" $ nfIO $ ((toList $ fromTest [||$$(getAgeT) "Drew"||]):: IO [(Int)]),
	    				  bench "HLINQ unknown name " $ whnfIO $ fromTest [||$$(getAgeT) "Ethan"||], 
	    				  bench "HLINQ unknown name " $ nfIO $ ((toList $ fromTest [||$$(getAgeT) "Ethan"||]):: IO [(Int)]), 
	    				  bench "HaskellDB Drew" $ whnfIO $ (HDB.getAge "Drew"), 
	    				  bench "HaskellDB unknown name " $ whnfIO $ (HDB.getAge "Ethan")
	    				],

	    bgroup "Compose" [ bench "HLINQ Edna Drew" $ whnfIO $ fromTest [||$$(composeT) "Edna" "Drew"||],
	    				   bench "HLINQ Edna Drew to tuples" $ nfIO $ ((toList $ fromTest [||$$(composeT) "Edna" "Drew"||]):: IO [(String)]),
	    				   bench "HLINQ Edna Drew to tuples fmap" $ nfIO $ ((toList' $ fromTest [||$$(composeT) "Edna" "Drew"||]):: IO [(String)]),
	    				   bench "HLINQ Edna Drew to tuples fmap'" $ nfIO $ ((toList'' $ fromTest [||$$(composeT) "Edna" "Drew"||]):: IO [(String)]),
	    				   bench "HLINQ Bob Tim" $ whnfIO $ fromTest [||$$(composeT) "Bob" "Tim"||],
	    				   bench "HLINQ Bob Tim to tuples" $ nfIO $ ((toList $ fromTest [||$$(composeT) "Bob" "Tim"||]):: IO [(String)]),
	    				   bench "HLINQ Bob Tim to tuples fmap" $ nfIO $ ((toList' $ fromTest [||$$(composeT) "Bob" "Tim"||]):: IO [(String)]),
	    				   bench "HLINQ Bob Tim to tuples fmap'" $ nfIO $ ((toList'' $ fromTest [||$$(composeT) "Bob" "Tim"||]):: IO [(String)]),
	    				   bench "HaskellDB Edna Drew" $ nfIO $ HDB.compose "Edna" "Drew",
	    				   bench "HaskellDB Bob Tim" $ whnfIO $ HDB.compose "Bob" "Tim"
	    				   ],

	    bgroup "Satisfies" [ bench "LINQ" $ whnfIO $ fromTestUntyped [|$satisfies $pre|],
	    					 bench "TLINQ" $ whnfIO $ fromTest [||$$satisfiesT $$preT||]
	    				   ],

	    bgroup "Satisfies Dynamic" [ bench "TLINQ" $ whnfIO $ fromTest [||$$satisfiesT $$(p t0)||]
	    						   ],
	    bgroup "Normalisation" [ bench "compose normalise" $ whnfIO $ normalisedCompose print,
	    						 bench "differences normalise" $ whnfIO $ normalisedDifferences print,
	    						 bench "compose normalisation" $ whnfIO $ printNormalised [||$$(composeT) "Edna" "Drew"||],
	    						 bench "differences normalisation" $ whnfIO $ printNormalised [||$$differencesT||]
	    					   ]
	]


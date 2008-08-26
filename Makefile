all :
	runhaskell Setup configure --user
	runhaskell Setup build
	runhaskell Setup install
	cd example; ghc --make sample1

all :
	#runhaskell Setup configure --user
	runhaskell Setup build
	runhaskell Setup install
	ghc --make example

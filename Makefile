.PHONY: api.watch
api.watch:
	stack build --no-run-tests --fast LordOfTheRings \
		--ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
		--interleaved-output \
		--file-watch \
		$(EXTRA_STACK_FLAGS) \
		--exec "bash -c \"pkill LordOfTheRings-exe; DEVELOPMENT=true stack exec LordOfTheRings-exe &\"" \

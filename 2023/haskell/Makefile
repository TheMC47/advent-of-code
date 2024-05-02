dev:
	ghcid --command="cabal repl aoc2023:test:dev" --allow-eval

watch-test:
	ghcid --command="cabal repl aoc2023:test:dev" --allow-eval --test="Tests.watchTest"

.PHONY: run
run:
	cabal run aoc2023 -- $(filter-out $@,$(MAKECMDGOALS))

%:
	@:

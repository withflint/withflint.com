.DEFAULT_GOAL := dev

dev:
		@lsof -ti tcp:5000 | xargs kill -9 | true
		@fantomas ./ -r
		@$(MAKE) -C elm dev &
		dotnet watch --no-hot-reload --project backend run

fix:
		@$(MAKE) -C elm fix
		@fantomas ./ -r

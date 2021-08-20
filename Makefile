.DEFAULT_GOAL := dev

OUT:="../static/GIT_VERSION/elm.js"
export OUT

install:
		@yarn global add elm-review
		@yarn global add elm-format
		@yarn global add elm-live
		@dotnet tool install -g fantomas-tool
		@dotnet restore

dev:
		@lsof -ti tcp:5000 | xargs kill -9 | true
		@fantomas ./ -r
		@$(MAKE) -C elm dev &
		dotnet watch --no-hot-reload --project backend run

fix:
		@$(MAKE) -C elm fix
		@fantomas ./ -r

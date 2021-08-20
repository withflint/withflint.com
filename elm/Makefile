.DEFAULT_GOAL := dev

dev:
		@elm-format src --yes
		@elm-live src/Main.elm --no-server -- --output=${OUT}

fix:
		@elm-review --fix-all

build:
		@elm make src/Main.elm --optimize --output=${OUT}
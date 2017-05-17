all: Elmscrew.elm Elmscrew/*
	elm make Elmscrew.elm --output=js/elmscrew.js

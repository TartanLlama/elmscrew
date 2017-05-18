all: elm/*
	elm make elm/Main.elm --output=js/elmscrew.js

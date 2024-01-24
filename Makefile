serve-prod:
	npx shadow-cljs release :app
	cd ./public && python3 -m http.server

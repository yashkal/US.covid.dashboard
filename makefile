help: ## Prints help for targets with comments
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Builds container for app (run to download latest data)
	docker build --tag us_covid_dashboard .

run: ## Run application
	docker run -it -p 80:3838 us_covid_dashboard

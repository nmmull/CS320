# based on the Makefile for OCaml package Streaming

.PHONY: git-check-uncommited
git-check-uncommited:
	@git diff-index --quiet HEAD -- || (echo "Error: uncommited changes"; exit 1)

.PHONY: page-build
page-build: git-check-uncommited
	git checkout main
	dune build @doc
	git checkout -

.PHONY: page-test
page-test: page-build
	open _build/default/_doc/_html/index.html

.PHONY: page-update
page-update: git-check-uncommited page-build
	git checkout gh-pages
	rm -rf ./landing
	cp -r ./_build/default/_doc/_html/ ./landing
	if git diff-index --quiet HEAD -- ./landing; then \
		echo "No changes."; \
	else \
		git add ./landing; \
		git commit -m "Update page"; \
		git push origin gh-pages; \
	fi
	git checkout -

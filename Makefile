.PHONY: docs-build
docs-build:
	git checkout main
	dune build @doc
	git checkout -

.PHONY: git-check-uncommited
git-check-uncommited:
	@git diff-index --quiet HEAD -- || (echo "Error: uncommited changes"; exit 1)

.PHONY: docs-update
docs-update: git-check-uncommited docs-build
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

build-website:
	dune build @doc

open: build-website
	open _build/default/_doc/_html/index.html

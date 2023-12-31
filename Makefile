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
	rm -rf ./cs320-course-page
	cp -r ./_build/default/_doc/_html/ ./cs320-course-page
	if git diff-index --quiet HEAD -- ./cs320-course-page; then \
		echo "No changes."; \
	else \
		git add ./cs320-course-page; \
		git commit -m "Update docs."; \
		git push origin gh-pages; \
	fi
	git checkout -

build-website:
	dune build @doc

open: build-website
	open _build/default/_doc/_html/index.html

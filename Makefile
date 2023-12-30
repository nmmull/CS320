build-website:
	dune build @doc

open: build-website
	open _build/default/_doc/_html/index.html

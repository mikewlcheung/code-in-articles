all: html pdf

html: Airlines.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"Airlines.Rmd\", html_document(keep_md=TRUE))"

pdf: Airlines.md
	R --no-save --no-restore -e "library(rmarkdown); render(\"Airlines.md\", pdf_document())"

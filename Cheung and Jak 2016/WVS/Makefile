all: html pdf

html: WVS.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"WVS.Rmd\", html_document(keep_md=TRUE))"

pdf: WVS.md
	R --no-save --no-restore -e "library(rmarkdown); render(\"WVS.md\", pdf_document())"


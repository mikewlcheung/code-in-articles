all: html 

html: TPB.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"TPB.Rmd\", html_document(keep_md=TRUE))"

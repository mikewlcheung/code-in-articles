all: conduct_a_meta_analysis.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"conduct_a_meta_analysis.Rmd\", html_document(keep_md=TRUE, self_contained=FALSE)); render(\"conduct_a_meta_analysis.Rmd\", pdf_document(toc=TRUE))"

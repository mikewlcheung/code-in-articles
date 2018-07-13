all: S1 S2 S3

S1: Supplemental_materials_1.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"Supplemental_materials_1.Rmd\", html_document(toc=TRUE, keep_md=TRUE)); render(\"Supplemental_materials_1.Rmd\", pdf_document(toc=TRUE, keep_tex=TRUE), output_file=\"Supplemental_materials_1.pdf\")"


S2: Supplemental_materials_2.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"Supplemental_materials_2.Rmd\", html_document(toc=TRUE, keep_md=TRUE)); render(\"Supplemental_materials_2.Rmd\", pdf_document(toc=TRUE, keep_tex=TRUE), output_file=\"Supplemental_materials_2.pdf\")"

S3: Supplemental_materials_3.Rmd
	R --no-save --no-restore -e "library(rmarkdown); render(\"Supplemental_materials_3.Rmd\", html_document(toc=TRUE, keep_md=TRUE)); render(\"Supplemental_materials_3.Rmd\", pdf_document(toc=TRUE, keep_tex=TRUE), output_file=\"Supplemental_materials_3.pdf\")"

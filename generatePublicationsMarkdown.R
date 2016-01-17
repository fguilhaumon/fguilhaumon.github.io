# generate publications.Rmd
#

library('rorcid')



file.create("publications.Rmd")

cat('---
title: "Publications"
output: html_document
---
', file = "publications.Rmd")

#get orcid work data
papers <- works(orcid_id("0000-0003-4707-8932"))

myName <- "Guilhaumon"
nPapers <- length(papers$data[[1]])

#get 'by paper' data
papers.rmarkdown <- lapply(1:nPapers,function(p){

  dat <- papers$data

  authors <- dat$'work-contributors.contributor'[[p]]$'credit-name.value'
  title <- dat$'work-title.title.value'[[p]]
  journal <- dat$`journal-title.value`[[p]]
  year <- dat$`publication-date.year.value`[[p]]

  #need to get the journal from citation to get full name
#   citation <- dat$'work-citation.citation'[[p]]
#   initialFilter <- "journal = [{][:alpha:][}]"
#   citation <- strsplit(citation, split=",")[[1]]
#   journal <- citation[grep(initialFilter ,citation)]
#   journal <- strsplit(strsplit(journal,"[{]")[[1]][2],"[}]")[[1]][1]

  doi <- dat$'work-external-identifiers.work-external-identifier'[[p]][2]

  authors.me <- grep(myName,authors)
  authors[authors.me] <- paste0("__",authors[authors.me],"__")
  string.authors <- paste0(paste(authors[-length(authors)],collapse=", ")," and ",authors[length(authors)],". ")

  string <- paste0("__",(nPapers+1)-p,"__ . ",string.authors,year,". [",title,".](http://doi.org/",doi,"). ","_",journal,"_.")
  if(p!=nPapers) string <- paste0(string,"\n\n")
  if(p==nPapers) string <- paste0(string,"\n")
  return(string)

})#eo lapply

cat("\n",file = "publications.Rmd", append = TRUE)

lapply(papers.rmarkdown,function(x){cat(x,file = "publications.Rmd", append = TRUE)})
#cat('{: reversed="reversed"}',file = "publications.Rmd", append = TRUE)
cat("\n",file = "publications.Rmd", append = TRUE)

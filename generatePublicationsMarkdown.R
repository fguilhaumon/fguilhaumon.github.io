# generate publications.Rmd
#

yaml <- read.table("_output.yaml",sep=":",stringsAsFactors =FALSE)
params <- t(yaml[(grep("params",yaml[,1])+1):dim(yaml)[1],])
colnames(params) <- params[1,]
params <- params[-1,]
params <- sapply(params,function(x)gsub("^\\s+|\\s+$", "", x)) #http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
names(params) <- sapply(names(params),function(x)gsub("^\\s+|\\s+$", "", x))
params <- as.list(params)

if(params$pubIDType=="orcid") library('rorcid')
if(params$pubIDType=="scholar"){
  library('scholar')
  library('rvest')
	library('httr')
}

myName <- params$name

file.create("publications.Rmd")

cat('---
title: "Publications"
output :
  html_document :
    css : include/css/index.css
---
', file = "publications.Rmd")

if(params$pubIDType=="orcid"){

  #get orcid work data
  papers <- works(orcid_id(params$pubIDValu))

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

    authors.me <- grep(myName,authors,ignore.case = TRUE)
    authors[authors.me] <- paste0("__",authors[authors.me],"__")
    string.authors <- paste0(paste(authors[-length(authors)],collapse=", ")," and ",authors[length(authors)],". ")

    string <- paste0("__",(nPapers+1)-p,"__ . ",string.authors,year,". [",title,"](http://doi.org/",doi,"). ","_",journal,"_.")
    if(p!=nPapers) string <- paste0(string,"\n\n")
    if(p==nPapers) string <- paste0(string,"\n")
    return(string)
  })#eo lapply
}#eo if orcid

if(params$pubIDType=="scholar" & FALSE){

  papers <- get_publications(params$pubIDValu,flush=TRUE)
  tri <- rev(order(papers$year))
  papers <- papers[tri,]

  nPapers <- dim(papers)[1]

  papers.rmarkdown <- lapply(1:nPapers,function(p){

    cat("processing paper :",as.character(papers[p,"title"]),"\n")

    paper.Details.html <- read_html(paste0("https://scholar.google.com/citations?view_op=view_citation&hl=en&user=jJwtZT0AAAAJ&sortby=pubdate&citation_for_view=jJwtZT0AAAAJ:",papers[p,"pubid"]))
    #cat("OK\n")
    link <- html_attr(html_nodes(paper.Details.html,css=".gsc_title_link"),"href")
    #cat("OK link1\n")
    nodePDF <- as.character(html_nodes(paper.Details.html,css=".gsc_title_ggi"))
    if(length(nodePDF)!=0) link.pdf <- strsplit(nodePDF,"\"")[[1]][grep("http",strsplit(nodePDF,"\"")[[1]])]

    #aPDF <- html_nodes(nodePDF,"a")
    #link.pdf <- html_attr(,"href")
    #link.pdf <- "http://#"
    #cat("OK link\n")
    authors <- html_nodes(paper.Details.html,css=".gs_scl")[[grep("Authors",html_nodes(paper.Details.html,css=".gs_scl"))]] #as.character(papers[p,"author"])
    authors <- strsplit(html_text(html_nodes(authors,".gsc_value")),",")[[1]]
    authors <- sapply(authors,function(x)gsub("^\\s+|\\s+$", "", x))
    title <- as.character(papers[p,"title"])
    journal <- as.character(papers[p,"journal"])
    year <- as.character(papers[p,"year"])
    authors.me <- grep(myName,authors,ignore.case = TRUE)
    authors[authors.me] <- paste0("__",authors[authors.me],"__")
    string.authors <- paste0(paste(authors[-length(authors)],collapse=", ")," and ",authors[length(authors)],". ")
    if(length(nodePDF)!=0){
      stringPDF <- paste0(" [PDF](",link.pdf,")")
    }else{
      stringPDF <- ""
    }
    string <- paste0("__",(nPapers+1)-p,"__ . ",string.authors,year,". [",title,"](",link,"). ","_",journal,"_.",stringPDF)
    if(p!=nPapers) string <- paste0(string,"\n\n")
    if(p==nPapers) string <- paste0(string,"\n")
    cat("OK\n")
    return(string)

  })#eo lapply
}#eo if scholar

if(params$pubIDType=="scholar" & TRUE){

	papers <- get_publications(params$pubIDValu,flush=TRUE)
	tri <- rev(order(papers$year))
	papers <- papers[tri,]
	nPapers <- dim(papers)[1]
	papers$num <- nPapers:1
	papers$papersColors <- sample(c("","warning","danger","info","success"),nPapers,replace=TRUE)

	years <- unique(papers$year)

	lapply(years,function(y){

		cat('<div>\n',file = 'publications.Rmd', append = TRUE)
		cat('<h2>',y,'</h2>\n',file = 'publications.Rmd', append = TRUE)
		cat('<ul class="timeline">\n',file = 'publications.Rmd', append = TRUE)

		subPaps <- subset(papers,year==y)

		lapply(1:dim(subPaps)[1],function(p){

		cat("processing paper :",as.character(subPaps[p,"title"]),"\n")

		url_base <- paste0("http://scholar.google.com/citations?",
											 "view_op=view_citation&hl=en&citation_for_view=")
		url_tail <- paste(params$pubIDValu, as.character(subPaps[p,"pubid"]), sep=":")
		url <- paste0(url_base, url_tail)

		paper.Details.html <- read_html(GET(url, handle=getOption("scholar_handle")))

		#paper.Details.html <- read_html(paste0("http://scholar.google.com/citations?view_op=view_citation&hl=en&citation_for_view=",params$pubIDValu,":",as.character(papers[p,"pubid"])))
		#cat("OK\n")
		link <- html_attr(html_nodes(paper.Details.html,css=".gsc_title_link"),"href")
		#cat("OK link1\n")
		nodePDF <- as.character(html_nodes(paper.Details.html,css=".gsc_title_ggi"))
		if(length(nodePDF)!=0) link.pdf <- strsplit(nodePDF,"\"")[[1]][grep("http",strsplit(nodePDF,"\"")[[1]])]

		#aPDF <- html_nodes(nodePDF,"a")
		#link.pdf <- html_attr(,"href")
		#link.pdf <- "http://#"
		#cat("OK link\n")
		authors <- html_nodes(paper.Details.html,css=".gs_scl")[[grep("Authors",html_nodes(paper.Details.html,css=".gs_scl"))]] #as.character(papers[p,"author"])
		authors <- strsplit(html_text(html_nodes(authors,".gsc_value")),",")[[1]]
		authors <- sapply(authors,function(x)gsub("^\\s+|\\s+$", "", x))
		title <- as.character(subPaps[p,"title"])
		journal <- as.character(subPaps[p,"journal"])
		year <- as.character(subPaps[p,"year"])
		authors.me <- grep(myName,authors,ignore.case = TRUE)
		authors[authors.me] <- paste0("<b>",authors[authors.me],"</b>")
		string.authors <- paste0(paste(authors[-length(authors)],collapse=", ")," and ",authors[length(authors)],". ")
		if(length(nodePDF)!=0){
			stringPDF <- paste0(' <a class="btn btn-danger btn-xs" href="',link.pdf,'">PDF</a>')
		}else{
			stringPDF <- ""
		}

		cat('<li class="timeline-inverted">\n',file = 'publications.Rmd', append = TRUE)
		st <- paste0('<div class="timeline-badge ',subPaps$papersColors[p],'">',subPaps$num[p],'</div>\n')
		cat(st,file = 'publications.Rmd', append = TRUE)
		cat('<div class="timeline-panel">\n',file = 'publications.Rmd', append = TRUE)
		cat('<div class="timeline-heading">\n',file = 'publications.Rmd', append = TRUE)

		st <- paste0('<h4 class="timeline-title">',title,'</h4>\n')
		cat(st,file = 'publications.Rmd', append = TRUE)

		st <- paste0('<p><a class="btn btn-default btn-xs" href="',link,'" role="button">Editor website</a>',stringPDF,'</p>\n')
		cat(st,file = 'publications.Rmd', append = TRUE)
		cat('</div>\n',file = 'publications.Rmd', append = TRUE)

		cat('<div class="timeline-body">\n',file = 'publications.Rmd', append = TRUE)
		st <- paste0('<p>',string.authors,'</p>\n')
		cat(st,file = 'publications.Rmd', append = TRUE)
		st <- paste0('<h5>',journal,'</5>\n')
		cat(st,file = 'publications.Rmd', append = TRUE)
		cat('</div>\n',file = 'publications.Rmd', append = TRUE)
		cat('</div>\n',file = 'publications.Rmd', append = TRUE)
		cat('</div>\n',file = 'publications.Rmd', append = TRUE)
		#string <- paste0("__",(nPapers+1)-p,"__ . ",string.authors,year,". [",title,"](",link,"). ","_",journal,"_.",stringPDF)

		})#eo lapply papers

		cat('</ul>\n',file = 'publications.Rmd', append = TRUE)
		cat('</div>\n',file = 'publications.Rmd', append = TRUE)
		cat('\n',file = 'publications.Rmd', append = TRUE)

	})#eo lapply years

}#eo if scholar

cat("\n",file = "publications.Rmd", append = TRUE)

#lapply(papers.rmarkdown,function(x){cat(x,file = "publications.Rmd", append = TRUE)})
cat("\n",file = "publications.Rmd", append = TRUE)

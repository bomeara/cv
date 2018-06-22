
#' Pull in info from ORCID
#' @param id Your ORCID id
#' @return A list of info from ORCID
#' @export
GetInfoFromOrcid <- function(id="0000-0002-0337-5997") {
  me <- rorcid::orcid_id(id)
  me.pubs <- rorcid::works(me)
  journal.info <- subset(me.pubs, type=="JOURNAL_ARTICLE")
  journal.dois <- c()
  for (i in sequence(nrow(journal.info))) {
    info.df <- journal.info$`external-ids.external-id`[[i]]
    info.df <- subset(info.df, `external-id-type`=="doi")
    if(nrow(info.df)>=1) {
      journal.dois <- append(journal.dois, info.df$`external-id-value`[1])
    }
  }
  #my.dois <- rorcid::identifiers(rorcid::works(me))
  journals <- rcrossref::cr_cn(dois = journal.dois, format = "bibtex", .progress="text")
  # TO DO: Make sure all entries are bibtex
  #journals <- me.pubs$'work-citation.citation'[which(me.pubs$'work-type'=="JOURNAL_ARTICLE")]
  other.products.raw <- subset(me.pubs, type!="JOURNAL_ARTICLE")
  other.products.raw <- rorcid::orcid_works(id, put_code=other.products.raw$`put-code`)[[1]]
  other.products <- other.products.raw[[1]]$`work.citation.citation-value`
  activities <- rorcid::orcid_activities(id)[[1]]
  funding <- plyr::rbind.fill(activities$funding$group$`funding-summary`)
  funding <- funding[order(funding$`start-date.year.value`, decreasing = TRUE),]
  # funding.full <- data.frame()
  # for (i in sequence(nrow(funding))) {
  #   local.funding <- rorcid::orcid_fundings(id, put_code=funding$`put-code`[i])[[1]]
  # }
  #affiliations <- activities$affiliation
  education <- activities$educations$`education-summary`
  education <- education[order(education$'end-date.year.value', decreasing=TRUE),]

  employment <- activities$employments$`employment-summary`
  employment <- employment[order(employment $'start-date.year.value', decreasing=TRUE),]

  return(list(journals=journals, other.products=other.products, funding=funding, education=education, employment=employment, id=id))

}

#' Create a Markdown document from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @param emphasis.name The name to bold in the publications list. Presumably your own.
#' @param scholar.id Your ID on Google Scholar. NULL if you don't want to use this.
#' @param impact.story..id Your ID on ImpactStory. NULL if you don't want to use this.
#' @export
CreateMarkdown <- function(orcid.info = GetInfoFromOrcid(), outdir=tempdir(), emphasis.name="O'Meara", scholar.id="vpjEkQwAAAAJ", impact.story.id = "0000-0002-0337-5997") {
  CreateSummaryMarkdown(orcid.info, outdir)
  CreateEducationMarkdown(orcid.info, outdir)
	CreateEmploymentMarkdown(orcid.info, outdir)
	CreateFundingMarkdown(orcid.info, outdir)
  CreatePublicationsMarkdown(orcid.info, outdir, emphasis.name, scholar.id, impact.story.id)
  CreatePeopleMarkdown(outdir=outdir)
  CreateServiceMarkdown(outdir=outdir)
}

#' Create a Markdown document to summarize me
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @param publications.offset How to change the publication count.
#'
#' You may want an offset if some of your publications shouldn't count.
#' For example, I have two Nature "papers" that are actually corrigendia,
#' that is, corrections for errors. Shouldn't really count, I think.
#' @export
CreateSummaryMarkdown <- function(orcid.info, outdir=tempdir(), publications.offset=-2, prominent.pubs='*Science, Nature, Ann. Rev Ecology, Evolution & Systematics, Systematic Biology, Evolution*, etc.') {
  results <- data.frame(matrix(nrow=6, ncol=2))
  colnames(results) <- c("", "")
  results[1,1] <- '**Publications**'
  results[1,2] <- paste(length(orcid.info$journals)+publications.offset, " journal articles, including ", prominent.pubs, sep="")

  results[2,1] <- '**Teaching**'
  results[2,2] <- "Approximately 3 courses per year on average, ranging from large introductory biology courses to small graduate seminars"

  results[3,1] <- '**Mentoring**'
  people <- read.delim2(system.file("extdata", "people.txt", package="cv"), stringsAsFactors=FALSE)
  results[3,2] <- paste(sum(grepl("PhD student", people$Stage)), " PhD students, ", sum(grepl("Postdoc", people$Stage)), " postdocs, ", sum(grepl("Faculty", people$Stage)), " faculty, ", "and served on ", sum(grepl("Committee", people$Stage)), " graduate student committees", sep="")

  results[4,1] <- '**Service/Outreach**'
  results[4,2] <- 'Darwin Day TN advisor, co-organizer of women in science symposium, workshops, and other activities, co-organizer for scientific meetings, curator of R phylogenetics task view, instructor at workshops in Sweden, Switzerland, Brazil, and various US locations (Ohio, TN, NC)'

  results[5,1] <- '**Leadership**'
  results[5,2] <- 'Associate Head for Dept. of Ecology & Evolutionary Biology, 2016-present; Associate Director for the National Institute for Mathematical and Biological Synthesis, 2016-present; Code of Conduct Committee for SSE/SSB/ASN, 2018-present; Communications Director for the Society of Systematic Biologists, 2016-2017; Society of Systematic Biologists Council, 2012-2014; iEvoBio co-organizer, 2014-2016.'

  results[6,1] <- '**Funding**'
  total.funding <- 0
  for (i in sequence(nrow(orcid.info$funding))) {
     local.funding <- rorcid::orcid_fundings(orcid.info$id, put_code=orcid.info$funding$`put-code`[i])[[1]]
     total.funding <- total.funding + as.numeric(local.funding$amount$value)
   }
  results[6,2] <- paste("$",round((1e-6)*total.funding,2), "M in external support, including ", sum(grepl("National Science Foundation", orcid.info$funding$organization.name)), " NSF grants (including a CAREER grant) plus funding from iPlant and Encyclopedia of Life", sep="")

  scholar.id="vpjEkQwAAAAJ"
  impact.story.id = "0000-0002-0337-5997"
  g.profile <- scholar::get_profile(scholar.id)
  github.user <- jsonlite::fromJSON(txt="https://api.github.com/users/bomeara")
  i.profile <- jsonlite::fromJSON(txt=paste("https://impactstory.org/api/person/", impact.story.id, sep=""))
  i.sources <- i.profile$sources
  results[7,1] <- '**Altmetrics**'
  #results[7,2] <- paste("Number of citations = ", g.profile$total_cites, "; h-index = ", g.profile$h_index, "; ", github.user$public_repos, " public github repos; Erdős number = 4; papers have been saved ", subset(i.sources, source_name=="mendeley")$posts_count, " times in reference manager Mendeley, have been tweeted about ", subset(i.sources, source_name=="twitter")$posts_count, " times, and have been mentioned ", subset(i.sources, source_name=="news")$posts_count, " times in the news", sep="")
  results[7,2] <- paste("Number of citations = ", g.profile$total_cites, "; h-index = ", g.profile$h_index, "; ", github.user$public_repos, " public github repos; Erdős number = 4; papers have been tweeted about ", subset(i.sources, source_name=="twitter")$posts_count, " times, and have been mentioned ", subset(i.sources, source_name=="news")$posts_count, " times in the news", sep="")

  cat('\n\n## Summary\n\n ', file=paste(outdir, "/summary.md", sep=""), sep='\n', append=FALSE)
  cat(capture.output(knitr::kable(results, row.names=FALSE)), file=paste(outdir, "/summary.md", sep=""), sep='\n', append=TRUE)

}


#' Create a Markdown document of education from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreateEducationMarkdown <- function(orcid.info, outdir=tempdir()) {
		education.string <- '\n\n## Education'
		for (i in sequence(dim(orcid.info$education)[1])) {
			education.string <- paste(education.string, '\n\n', 	orcid.info$education[i,]$organization.name, ': ', orcid.info$education[i,]$'role-title', " (", orcid.info$education[i,]$'end-date.year.value', ")", sep='')
			if(!is.na(orcid.info$education$'department-name')[i]) {
				education.string <- paste(education.string, ' in ', 	orcid.info$education[i,]$'department-name', sep='')
			}
		}
		cat(education.string, file=paste(outdir, "/education.md", sep=""))
}

#' Create a Markdown document of employment history from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreateEmploymentMarkdown <- function(orcid.info, outdir=tempdir()) {
		employment.string <- '\n\n## Employment'
		orcid.info$employment$'end-date.year.value'[which(is.na(orcid.info$employment$'end-date.year.value'))] <- "Present"
		for (i in sequence(dim(orcid.info$employment)[1])) {
			employment.string <- paste(employment.string, '\n\n', 	orcid.info$employment[i,]$'start-date.year.value', '-', orcid.info$employment[i,]$'end-date.year.value', ": ", orcid.info$employment[i,]$'role-title', ", ", sep='')
			if(!is.na(orcid.info$employment$'department-name')[i]) {
				employment.string <- paste(employment.string, "Dept. of ", 	orcid.info$employment[i,]$'department-name', ", ", sep='')
			}
			employment.string <- paste(employment.string, orcid.info$employment[i,]$'organization.name', ", ", orcid.info$employment[i,]$'organization.address.city', ", ", orcid.info$employment[i,]$'organization.address.region', sep="")
		}
		cat(employment.string, file=paste(outdir, "/employment.md", sep=""))
}


#' Create a Markdown document of funding from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreateFundingMarkdown <- function(orcid.info, outdir=tempdir(), additional.te = "This is all in addition to other **funding my students have gotten** (NSF EAPSI grant, fellowships from NIMBioS and PEER (an NIH-funded program at UTK), Google Summer of Code funding), **funding for workshops or working groups** (from NIMBioS and the Society for Systematic Biologists), and **funding I got before my faculty position** (NESCent postdoctoral fellowship, NSF DDIG, NSF GRF, and various internal grants at UC Davis).") {
		funding.string <- '\n\n## Funding\n\n'
		funding.string <- paste(funding.string, additional.te, sep="")

    funding.full <- list()
    total.funding <- 0
    for (i in sequence(nrow(orcid.info$funding))) {
       local.funding <- rorcid::orcid_fundings(orcid.info$id, put_code=orcid.info$funding$`put-code`[i])[[1]]
       total.funding <- total.funding + as.numeric(local.funding$amount$value)
       funding.full[[i]] <- local.funding
     }

    funding.string <- paste(funding.string, paste(" Total external funding, so far, as a faculty member is $", prettyNum(sum(as.numeric(orcid.info$funding$amount.value)), big.mark=",", scientific=FALSE), ".", sep=""), sep="")

		funding.string <- paste(funding.string, '\n\n| Year | Title | Funder | Amount |\n| ---- | ------------------------------ | -------- | ------ |', sep="")
  #  orcid.info$funding <- orcid.info$funding[order(orcid.info$funding$'start-date.year.value', decreasing=TRUE),]
  	for (i in sequence(length(funding.full))) {

			organization <- funding.full[[i]]$organization$name
			if (grepl("National Science Foundation", organization, ignore.case=FALSE)) {
				organization <- 'NSF'
			}
			if (grepl("National Institutes of Health", organization, ignore.case=FALSE)) {
				organization <- 'NIH'
			}
			funding.string <- paste(funding.string, '\n| ', funding.full[[i]]$`start-date`$year$value, ' | ', funding.full[[i]]$title$title$value, ' | ', organization, ' | $', prettyNum(as.numeric(funding.full[[i]]$amount$value), big.mark=",", scientific=FALSE), ' |', sep="")


	#		funding.string <- paste(funding.string, '\n\n', orcid.info$funding[i,]$'funding-title.title.value', " ",	orcid.info$funding[i,]$organization.name, ': ', orcid.info$funding[i,]$'role-title', " (", orcid.info$funding[i,]$'end-date.year.value', "): $", orcid.info$funding[i,]$'amount.value', sep='')
		#	if(!is.null(orcid.info$funding$'department-name')[i]) {
		#		funding.string <- paste(funding.string, ' in ', 	orcid.info$funding[i,]$'department-name', sep='')
		#	}
		}
		cat(funding.string, file=paste(outdir, "/funding.md", sep=""))
}

#' Apostophes cause problems. This will fix them.
#' @param citations Bibtext formatted, presumably orcid.info$journals
#' @return Same object, but with curly quotes and similar evil symbols fixed
#' @export
CleanNames <- function(citations) {
	for (i in sequence(length(citations))) {
    citations[i] <- gsub("meara", "Meara", citations[i])
		citations[i] <- gsub("\\{'\\}", "'", citations[i])
		citations[i] <- gsub("\\{\\\\textquotesingle\\}", "'", citations[i])
		citations[i] <- gsub("O\\?Meara", "O'Meara", citations[i])
		citations[i] <- gsub('’', "'", citations[i])
		citations[i] <- gsub("\\{\\\\\\^\\a\\}\\?\\?", "'", citations[i])
	}
	return(citations)
}



#' Create a Markdown document of people in the lab from biographical info
#' @param infile The path to the text delimited file
#' @param outdir The directory to store the markdown file in
CreatePeopleMarkdown <- function(infile =   system.file("extdata", "people.txt", package="cv"), outdir=tempdir()) {
  people <- read.delim2(infile, stringsAsFactors=FALSE)
  people$Stop <- as.character(people$Stop)
  people$Stop[is.na(people$Stop)] <- "present"
  people$Duration <- paste(people$Start,"-",people$Stop, sep="")
  people$Name <- paste(people$First,people$Last)
  for (i in sequence(dim(people)[1])) {
    if(nchar(people$URL[i])>3) {
      people$Name[i] <- paste("[", people$First[i], " ",people$Last[i],"](",people$URL[i], ")", sep="")
    }
  }

  cat('\n\n## Mentoring, Faculty\n\nOur department now has faculty mentored by a committee of later career faculty. I have served on several', file=paste(outdir, "/people.md", sep=""), sep='\n', append=FALSE)
  faculty <- subset(people, Stage=="Faculty")
  faculty <- faculty[order(faculty$Last),]
  faculty.pretty <- faculty[,c("Name", "Department")]
  cat(capture.output(knitr::kable(faculty.pretty, row.names=FALSE)), file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)

  cat('\n\n## Mentoring, Postdocs\n\nI have mentored numerous postdocs off of my own grants and/or as one of their chosen NIMBioS mentors. Note that NIMBioS postdocs pursue independent research projects but choose one faculty member to mentor them in math and another to mentor them in biology (I have served in both roles).', file=paste(outdir, "/people.md", sep=""), sep='\n', append=FALSE)
  postdocs <- subset(people, Stage=="Postdoc")
  postdocs <- postdocs[order(postdocs$Last),]
  postdocs.pretty <- postdocs[,c("Name", "Duration", "NIMBioS", "CurrentPosition")]
  names(postdocs.pretty)[4] <- "Current Position"
  cat(capture.output(knitr::kable(postdocs.pretty, row.names=FALSE)), file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)

  cat('\n\n## Mentoring, Grad students in my lab\n\n ', file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)
  grads <- subset(people, Stage=="PhD student")
  grads <- grads[order(grads$Last),]
  grads.pretty <- grads[,c("Name","Stage", "Duration", "Note")]
  names(grads.pretty)[3] <- "Time in Lab"
  cat(capture.output(knitr::kable(grads.pretty, row.names=FALSE)), file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)

  cat('\n\n## Mentoring, Undergrad students in my lab\n\n ', file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)
  undergrads <- subset(people, Stage=="Undergrad")
  undergrads <- undergrads[order(undergrads $Last),]
  undergrads.pretty <- undergrads[,c("Name","Stage", "Duration", "Note")]
  names(undergrads.pretty)[3] <- "Time in Lab"
  cat(capture.output(knitr::kable(undergrads.pretty, row.names=FALSE)), file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)

  cat('\n\n## Mentoring, Grad student committees\n\nIn addition to my own students, of course.', file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)
  com <- subset(people, Stage=="Committee")
  com <- com[order(com$Last),]
  com.pretty <- com[,c("Name","Department")]
  cat(capture.output(knitr::kable(com.pretty, row.names=FALSE)), file=paste(outdir, "/people.md", sep=""), sep='\n', append=TRUE)


}

#' Create a Markdown document of service from biographical info
#' @param infile The path to the text delimited file
#' @param outdir The directory to store the markdown file in
CreateServiceMarkdown <- function(infile =   system.file("extdata", "service.txt", package="cv"), outdir=tempdir()) {
  service <- read.delim2(infile, stringsAsFactors=FALSE)
  cat('\n\n## Service\n\n', file=paste(outdir, "/service.md", sep=""), sep='\n', append=FALSE)
  service$Service <- paste("*", service$Service)
  cat(service$Service, file=paste(outdir, "/service.md", sep=""), sep='\n', append=TRUE)
}


#' Get collaborators from past time period
#' @param orcid.info The list of info from orcid
#' @param starting.year What year to start including collaborators
#' @param outdir The directory to store the temporary markdown file in
#' @return data.frame with the authors and the papers in the time period
#' @export
GetCollaborators <- function(orcid.info, starting.year=2013, outdir=tempdir()) {
  lapply(CleanNames(orcid.info$journals), write,  paste(outdir, "/publications.bib", sep=""), append=TRUE)
	publications <- RefManageR::ReadBib(paste(outdir, "/publications.bib", sep=""))
	publications <- sort(publications, decreasing=TRUE, sorting="ynt")
  publications <- publications[which(as.numeric(publications$year)>=starting.year)]
  CollapseName <- function(x) {
    x <- (unlist(x))
    return(paste0(x[grepl("family", names(x))], ", ", paste(x[grepl("given", names(x))], collapse=" ")))
  }
  all.authors <- c()
  all.publications <- c()
  for (i in sequence(length(publications))) {
    all.authors <- append(all.authors, sapply(publications[i]$author, CollapseName))
    all.publications <- append(all.publications, rep(paste0(publications[i]$title, " (", publications[i]$year, ")"), length(publications[i]$author)))
  }
  authors.df <- data.frame(coauthor=all.authors, publication=all.publications, stringsAsFactors=FALSE)
  all.aggregation <- aggregate(authors.df, by=list(authors.df$coauthor), FUN=paste, collapse="; ")[,-2]
  names(all.aggregation)[1] <- "coauthor"
  all.aggregation[,2] <- as.character(all.aggregation[,2])
  #all.authors <- sort(all.authors)

  #all.authors.table <- table(all.authors)
  #all.authors <- unique(all.authors)
  return(all.aggregation)
}


#' Create a Markdown document of publications from orcid
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @param emphasis.name The name to bold in the publications list. Presumably your own.
#' @param scholar.id Your ID on Google Scholar. NULL if you don't want to use this.
#' @param impact.story..id Your ID on ImpactStory. NULL if you don't want to use this.
#' @param badges Vector of ImpactStory badge names you want to show (a lot are goofy: could do c('global_reach', 'depsy')).
#' @export
CreatePublicationsMarkdown <- function(orcid.info, outdir=tempdir(), emphasis.name = "O'Meara", scholar.id="vpjEkQwAAAAJ", impact.story.id = "0000-0002-0337-5997", badges=c()) {
  lapply(CleanNames(orcid.info$journals), write,  paste(outdir, "/publications.bib", sep=""), append=TRUE)
	publications <- RefManageR::ReadBib(paste(outdir, "/publications.bib", sep=""))
	publications <- sort(publications, decreasing=TRUE, sorting="ynt")
  publications.text <- capture.output(print(publications, .opts=list(bib.style="authoryear", dashed=FALSE, max.names=100, style="markdown", sorting="none", no.print.fields=c("URL", "DOI"))))
  publications.text <- gsub(emphasis.name, paste('**', emphasis.name, '**', sep=""), publications.text)

  cat(CleanNames(orcid.info$other.products), file=paste(outdir, "/chapters.bib", sep=""))
  chapters <- RefManageR::ReadBib(paste(outdir, "/chapters.bib", sep=""))
  chapters <- sort(chapters, decreasing=TRUE, sorting="ynt")
  chapters.text <- capture.output(print(chapters, .opts=list(bib.style="authoryear", dashed=FALSE, max.names=100, style="markdown", sorting="none", no.print.fields=c("URL", "DOI"))))
  chapters.text <- gsub(emphasis.name, paste('**', emphasis.name, '**', sep=""), chapters.text)

  cat('\n\n## Publications: Papers', file=paste(outdir, "/publications.md", sep=""), sep='\n', append=FALSE)
  if(!is.null(scholar.id)) {
    g.profile <- NULL
    try(g.profile <- scholar::get_profile(scholar.id))
    if(!is.null(g.profile)) {
      cat(paste('\n\nAccording to Google Scholar, my work has been cited ', g.profile$total_cites, " times, and my h-index is ", g.profile$h_index, ". (Google Scholar tends to overestimate citations, however).", sep=""),  file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
    }
  }

  if(!is.null(impact.story.id)) {
    i.profile <- NULL
    try(i.profile <- jsonlite::fromJSON(txt=paste("https://impactstory.org/api/person/", impact.story.id, sep="")))
    if(!is.null(i.profile)) {
      if(any(i.profile$badges$name %in% badges)) {
        cat(paste('\n\nAccording to NSF-funded [ImpactStory.org](https://impactstory.org/u/', impact.story.id, '), a source of altmetrics data (a measure of impact beyond citations), my work has various impacts:',   sep=""),  file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
        for (badge.index in sequence(length(badges))) {
          if(badges[badge.index] %in% i.profile$badges$name) {
            matching.row <- which(i.profile$badges$name==badges[badge.index])
            cat(paste('\n* ', gsub("Your", "My", gsub("your", "my", i.profile$badges$description[matching.row])), " ",i.profile$badges$context[matching.row],   sep=""),  file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)

          }
        }
      }
    }
  }
  #cat('\n\n###Papers', file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
  cat('\n\n', file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
  cat(publications.text, file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
  cat('\n\n## Publications: Books or Book Chapters\n\n', file=paste(outdir, "/publications.md", sep=""), append=TRUE)
  cat(chapters.text, file=paste(outdir, "/publications.md", sep=""), sep='\n', append=TRUE)
}


#' Compile a set of markdown documents and convert with pandoc
#' @param input Vector of markdown documents
#' @param outdir The directory to store the output in
#' @param output The output base file name. You'll receive <output>.pdf and <output>.html files.
#' @param css The css file with formatting info.
#' @export
#FinalCompileCV <- function(input = c("head.md", "summary.md", "education.md", "employment.md", "publications.md", "teaching.md", "funding.md", "service.md", "postdocs.md", "gradstudents.md", "undergradstudents.md", "gradcommittees.md", "software.md", "presentations.md"), output="OMearaCV.pdf") {
FinalCompileCV <- function(input = c(system.file("extdata", "head.md", package="cv"), "summary.md", "education.md", "employment.md", "publications.md", system.file("extdata", "teaching.md", package="cv"), "funding.md", system.file("extdata", "presentations.md", package="cv"), "people.md", "service.md"), outdir=tempdir(), css = system.file("extdata", "format.css", package="cv"), output="OMearaCV") {
  original.wd <- getwd()
  setwd(outdir)
  system(paste("pandoc --css ", css, " -o ", output, ".html ", paste(input, collapse=" "), sep=""))
  print(paste("HTML file ", output, ".html has been created in ", outdir, sep=""))
  system(paste("wkhtmltopdf ", output, ".html ", output, ".pdf", sep=""))
  print(paste("PDF file ", output, ".pdf has been created in ", outdir, sep=""))
  #system(paste("pandoc --css ", css, " -o ", output, "_pandoc.pdf ", paste(input, collapse=" "), sep=""))
  system(paste("pandoc --css ", css, " -o ", output, ".md ", paste(input, collapse=" "), sep=""))
  system(paste("pandoc --css ", css, " -o ", output, ".docx ", paste(input, collapse=" "), sep=""))
  #system(paste("pandoc -o ", output, "_fromword.pdf ", paste0(output, ".docx"), sep=""))
  setwd(original.wd)
}

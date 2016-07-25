
#' Pull in info from ORCID
#' @param id Your ORCID id
#' @return A list of info from ORCID
#' @export
GetInfoFromOrcid <- function(id="0000-0002-0337-5997") {
  me <- rorcid::orcid_id(id)
  me.pubs <- rorcid::works(me)$data
  # TO DO: Make sure all entries are bibtex
  journals <- me.pubs$'work-citation.citation'[which(me.pubs$'work-type'=="JOURNAL_ARTICLE")]
  books <- me.pubs$'work-citation.citation'[which(me.pubs$'work-type'=="BOOK")]
  activities <- me[[1]]$`orcid-activities`
  funding <- activities$`funding-list`$funding
  affiliations <- activities$`affiliations`$affiliation
  education <- subset(affiliations, type=="EDUCATION")
  education <- education[order(education$'end-date.year.value', decreasing=TRUE),]

  employment <- subset(affiliations, type=="EMPLOYMENT")
  employment <- employment[order(employment $'start-date.year.value', decreasing=TRUE),]

  return(list(journals=journals, books=books, funding=funding, education=education, employment=employment))

}

#' Create a Markdown document from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreateMarkdown <- function(orcid.info = GetInfoFromOrcid(), outdir=".") {
	CreateEducationMarkdown(orcid.info, outdir)
	CreateEmploymentMarkdown(orcid.info, outdir)
	CreateFundingMarkdown(orcid.info, outdir)

}

#' Create a Markdown document of education from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreateEducationMarkdown <- function(orcid.info, outdir=".") {
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
CreateEmploymentMarkdown <- function(orcid.info, outdir=".") {
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
CreateFundingMarkdown <- function(orcid.info, outdir=".", additional.te = "This is all in addition to other **funding my students have gotten** (NSF EAPSI grant, fellowships from NIMBioS and PEER (an NIH-funded program at UTK), Google Summer of Code funding), **funding for workshops or working groups** (from NIMBioS and the Society for Systematic Biologists), and **funding I got before my faculty position** (NESCent postdoctoral fellowship, NSF DDIG, NSF GRF, and various internal grants at UC Davis).") {
		funding.string <- '\n\n## Funding\n\n'
		funding.string <- paste(funding.string, additional.te, sep="")
		funding.string <- paste(funding.string, '\n\n| Year | Title | Funder | Amount |\n| ---- | ------------- | -------- | ------ |', sep="")
		for (i in sequence(dim(orcid.info$funding)[1])) {
			organization <- orcid.info$funding[i,]$organization.name
			if (grepl("National Science Foundation", organization, ignore.case=FALSE)) {
				organization <- 'NSF'
			}
			if (grepl("National Institutes of Health", organization, ignore.case=FALSE)) {
				organization <- 'NIH'
			}
			funding.string <- paste(funding.string, '\n| ', orcid.info$funding[i,]$'end-date.year.value', ' | ', orcid.info$funding[i,]$'funding-title.title.value', ' | ', organization, ' | ', orcid.info$funding[i,]$'amount.value', ' |', sep="")
			

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
		citations[i] <- gsub("\\{'\\}", "'", citations[i])
		citations[i] <- gsub("\\{\\\\textquotesingle\\}", "'", citations[i])
		citations[i] <- gsub("O\\?Meara", "O'Meara", citations[i])
		citations[i] <- gsub('’', "'", citations[i])
		citations[i] <- gsub("\\{\\\\\\^\\a\\}\\?\\?", "'", citations[i])
		citations[i] <- gsub("meara", "Meara", citations[i])
	}
	return(citations)
}

#' Create a Markdown document of funding from biographical info
#' @param orcid.info The list of info from orcid
#' @param outdir The directory to store the markdown file in
#' @export
CreatePublicationsMarkdown <- function(orcid.info, outdir=".") {
	cat(CleanNames(orchid.info$journals), file=paste(outdir, "/publications.bib", sep=""))
	publications <- ReadBib(paste(outdir, "/publications.bib", sep=""))
	publications <- sort(publications, decreasing=TRUE, sorting="ynt")
}

#' Compile a set of markdown documents and convert with pandoc
#' @param input Vector of markdown documents
#' @param output The output file name. Pandoc will use the extension info to create a file of the right type.
#' @export
FinalCompileCV <- function(input = c("head.md", "summary.md", "education.md", "employment.md", "publications.md", "teaching.md", "funding.md", "service.md", "postdocs.md", "gradstudents.md", "undergradstudents.md", "gradcommittees.md", "software.md", "talks.md"), output="OMearaCV.pdf") {
  system(paste("pandoc --filter=pandoc-citeproc --standalone -o ", output, " ", paste(input, collapse=" "), sep=""))
}

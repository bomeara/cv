

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

CreateMarkdown <- function(orcid.info = GetInfoFromOrcid(), outdir=".") {
	CreateEducationMarkdown(orcid.info, outdir)
	CreateEmploymentMarkdown(orcid.info, outdir)

		
}

CreateEducationMarkdown <- function(orcid.info, outdir=".") {
		education.string <- '## Education'
		for (i in sequence(dim(orcid.info$education)[1])) {
			education.string <- paste(education.string, '\n\n', 	orcid.info$education[i,]$organization.name, ': ', orcid.info$education[i,]$'role-title', " (", orcid.info$education[i,]$'end-date.year.value', ")", sep='')
			if(!is.na(education$'department-name')[i]) {
				education.string <- paste(education.string, ' in ', 	orcid.info$education[i,]$'department-name', sep='')
			}
		}
		cat(education.string, file=paste(outdir, "/education.md", sep=""))
}


CreateEmploymentMarkdown <- function(orcid.info, outdir=".") {
		employment.string <- '## Employment'
		orcid.info$employment$'end-date.year.value'[which(is.na(employment$'end-date.year.value'))] <- "Present"
		for (i in sequence(dim(orcid.info$employment)[1])) {
			employment.string <- paste(employment.string, '\n\n', 	orcid.info$employment[i,]$'start-date.year.value', '-', orcid.info$employment[i,]$'end-date.year.value', ": ", orcid.info$employment[i,]$'role-title', ", ", sep='')
			if(!is.na(employment$'department-name')[i]) {
				employment.string <- paste(employment.string, "Dept. of ", 	orcid.info$employment[i,]$'department-name', ", ", sep='')
			}
			employment.string <- paste(employment.string, orcid.info$employment[i,]$'organization.name', ", ", orcid.info$employment[i,]$'organization.address.city', ", ", orcid.info$employment[i,]$'organization.address.region', sep="")
		}
		cat(employment.string, file=paste(outdir, "/employment.md", sep=""))	
}

FinalCompileCV <- function(input = c("head.md", "summary.md", "education.md", "employment.md", "publications.md", "teaching.md", "grants.md", "service.md", "postdocs.md", "gradstudents.md", "undergradstudents.md", "gradcommittees.md", "software.md", "talks.md"), output="OMearaCV.pdf") {
  system(paste("pandoc ", paste(input, collapse=" "), " > ", output, sep=""))
}

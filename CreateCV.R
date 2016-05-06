GetInfoFromOrcid <- function(id="0000-0002-0337-5997") {
  me <- rorcid::orcid_id(id)
  me.pubs <- rorcid::works(me)$data
  # TO DO: Make sure all entries are bibtex
  journals <- me.pubs$'work-citation.citation'[which(me.pubs$'work-type'=="JOURNAL_ARTICLE")]
  books <- me.pubs$'work-citation.citation'[which(me.pubs$'work-type'=="BOOK")]

}

FinalCompileCV <- function(input = c("head.md", "summary.md", "education.md", "employment.md", "publications.md", "teaching.md", "grants.md", "service.md", "postdocs.md", "gradstudents.md", "undergradstudents.md", "gradcommittees.md", "software.md", "talks.md"), output="OMearaCV.pdf") {
  system(paste("pandoc ", paste(input, collapse=" "), " > ", output, sep=""))
}

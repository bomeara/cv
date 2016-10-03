# cv
This is an R package for creating a CV using information from Google Scholar, GitHub, ORCID, and ImpactStory.

To install

```{r}
install.packages("devtools")
devtools::install_github("bomeara/cv")
```

You will also need wkhtmltopdf and pandoc installed on your system (for Mac users, look into MacPorts for this; Brew may also work).

Once you've installed the package,

```{r}
library(cv)
CreateMarkdown(GetInfoFromOrcid(id="0000-0002-0337-5997"), emphasis.name="O'Meara", scholar.id="vpjEkQwAAAAJ", impact.story.id = "0000-0002-0337-5997")
FinalCompileCV(output="OMearaCV")
```

You'll probably want to change the id and names to your own rather than mine (for one thing, this could make it more likely to get the job you want). There are also files in /inst/extdata you will need to change.

You can see an example of the output at http://www.brianomeara.info/cv.

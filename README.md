![Image of Cambrian Ecosystem](http://www.dwmiller.net/pics/ediacara.jpg)
# *The Ecophysiological Influences on the Body Size of Marine Animal Genera through the Phanerozoic*
This is the repository that the SEYI: Biodiversity Internship program will use for our analyses and writings on body size evolution.

## Sections of Paper:
- ***[Abstract](https://docs.google.com/document/d/1MDUCv2TA2581vGZcRmPJDk0expJSdexeuWW4dmC9sd0/edit?usp=sharing)*** (opens in Google Docs)

- ***[Phyla Research](https://github.com/naheim/paleosizePaper/tree/master/phyla)***

- ***[Tiering Research](https://github.com/naheim/paleosizePaper/tree/master/tiering)***

- ***[Motility Research](https://github.com/naheim/paleosizePaper/tree/master/motility)***

- ***[Feeding Mode Research](https://github.com/naheim/paleosizePaper/tree/master/feeding)***

- ***[Respiratory Anatomy Research](https://github.com/naheim/paleosizePaper/tree/master/respAnatomy)***

## Utility Folders:
- ***[Raw Data Files](https://github.com/naheim/paleosizePaper/tree/master/rawDataFiles)***

- ***[Example Code](https://github.com/naheim/paleosizePaper/tree/master/exampleCode)***

- ***[References](https://github.com/naheim/paleosizePaper/tree/master/references)***

## *Figure Formatting Guidelines:*

Color Scheme: c("#ff5640","#ffd900","#00ffd7","#ee92ed","#ff00ff","#0000ff")

### General Standards:
> timescale = rectangle

> rest = square

> Code for Rectangular Time.plot: cex.lab = 1.2, mar = c(4.5,4.5,4.5,4.5)+0.1, mgp=c(3, 0.75, 0), cex.axis = 1.25)

> Code for Nice, Adjustable Title: mtext(side=3, line=0.5, "YOUR TITLE HERE”, col="black", font=4, cex=2)

> Legends are Regular Sized: CEX = 1

> Labels: White background and White Borders

> No Extinction Lines: Box Plots and Proportion Plots

> Exact Dates for Mass Extinctions: 443.8, 358.9, 252.17, 201.3, 66

#### Box Plot Standards:
>  Boxplots should have notches (add: notch=TRUE)

> For X Labels, Only Name of Catagory (ex. Pelagic, Water-Breathing Closed System, etc.)

#### Plot Decoration Standards:
> Mass Extinctions --> dashed lines (lty=5)

> Mass Extinction Lines Color: azure4

> Line Graphs have "3" boldness

# *Key References*
### Body Size Data:
* Heim, N. A., M. L. Knope, E. K. Schaal, S. C. Wang, and J. L. Payne. 2015. Cope’s rule in the evolution of marine animals. Science 347:867–870. doi: 10.1126/science.1260065.

### Tiering, Motility & Feeding Data
* Knope, M. L., N. A. Heim, L. O. Frishkoff, and J. L. Payne. 2015. Limited role of functional differentiation in early diversification of animals. Nature Communications 6:6455. doi: 10.1038/ncomms7455.

### Respiratory Anatomy
* These are newly collected data, compiled from the secondary literature. Don't worry about citing this data right now.  Individual citations will be included with the data file we submit to the journal.

### The geological timescale
* Cohen, K.M., S. C. Finney, P. L. Gibbard, and J.-X. Fan. 2013. The ICS International Chronostratigraphic Chart. Episodes 36:199-204.

### R
* base program
	* R Core Team. 2018. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

* paleoTS
	* Hunt, G. 2015. paleoTS: Analyze Paleontological Time-Series. R package version 0.5-1. https://CRAN.R-project.org/package=paleoTS.


# Formatting Instructions for *Paleobiology*

Our project for this summer is to begin drafting a paper for publication as a 'Reserach Article` in the journal *Paleobiology*, which is the premier journal for quantitiaive studies of the fossil record.

Below are the journal's instruction to authors on formatting and submission precedures. Please read throught the sections to understand the format and styling you should be using.

## [Full Instructions](https://www.cambridge.org/core/journals/paleobiology/information/instructions-contributors)

#### Title

> Must be informative and under 50 words.

#### Manuscript Text

> Please submit manuscript text as a stand-alone MS Word .doc or .docx file. Manuscripts cannot not contain appendices, tables, figures, images, or supplemental data files. These are uploaded as separate file(s).

> Manuscripts should be double spaced using 12-point font, preferably Times New Roman. In addition, please number all pages, and all lines. Submissions are expected to be between 20 and 50 manuscript pages (including text, references, figures, and tables).

#### Tables

> Please submit each individual table as a MS Word table (.doc or .docx) or MS Excel table (.xls or .xlsx). When including a Word table, please ensure that it is created using Word's table function, rather than a tab-delineated table. Each table must be a separate file and should include its own caption. Tables should be ordered as they are cited in the text. Long tables, particularly those with raw data, should be formatted for the Supplementary Materials (see below).

#### Figures

> Please include figures with a resolution of 600 dpi or higher. Line drawings should be submitted at 1200 dpi.

> TIFF files are recommended for all submitted figures; however, Paleobiology can also accept .eps and .jpeg files. Please visit our [figure guide](https://www.cambridge.org/core/services/aop-file-manager/file/57611f60c0a3284826389eda).

#### References

> Paleobiology follows The Chicago Manual of Style, available [here](http://www.chicagomanualofstyle.org/home.html). EndNote has created an output style for *Paleobiology* to assist in formatting references. It can be downloaded [here](http://endnote.com/downloads/style/paleobiology). Please note that a one-to-one correspondence must exist between works cited in the text and listed in the Literature Cited section. Books or manuscripts in press may be included; if possible, include the anticipated year of publication. Unpublished manuscripts, manuscripts in review, or otherwise unpublished data should not be cited. Avoid citing unpublished theses or dissertations.

> The authors are responsible for the accuracy of all citations. Please ensure that literature is cited in the text in chronological order, by the last name of the author(s) and the date of publication. For works with three or more authors, the last name of the senior author is followed by "et al."

### Geological Time

> For geologic time, please follow the [International Commission on Stratigraphy Time Chart](http://www.stratigraphy.org/index.php/ics-chart-timescale). For conventions in stratigraphic terms, please follow the [International Commission on Stratigraphy](http://www.stratigraphy.org/index.php/ics-stratigraphicguide). Please be sure to differentiate between geologic dates and duration of time. Use the abbreviations Ga, Ma, and Ka to indicate dates (billions, millions, and thousands of years before the present, respectively). Use Gyr, Myr, and Kyr to indicate duration of time.

> "Upper" and "lower" refer to rock or time-stratigraphic units; "late" and "early" refer to time. Use lower case when the age constraints are not known, generalizations are made, or when no formal subdivision exists.

> Please capitalize the names of formal time units or time-stratigraphic units. "Early/Lower Cretaceous" (for periods/systems), but "late/upper Miocene," "early Paleozoic," "early/lower Albian" (for informal subdivision of stage/age/epoch/series).

> For reference, please use [Gradstein et al. 2012](http://www.sciencedirect.com/science/book/9780444594259). or the current [ICS stratigraphic chart](http://www.stratigraphy.org/index.php/ics-chart-timescale), as guides.

> Use a slash (/) to denote boundaries, and an en-dash (–) to denote time ranges.

> As an example:

>	* (K/T boundary)
> 	* (Eocene–Oligocene mammals)


## Reference Examples From the Chicago Manual of Style
[[Original CMS Text](http://www.chicagomanualofstyle.org/tools_citationguide/citation-guide-2.html)]

### Book

***Reference list entries (in alphabetical order)***

* Grazer, B., and C. Fishman. 2015. *A Curious Mind: The Secret to a Bigger Life*. New York: Simon & Schuster.

* Smith, Z. 2016. *Swing Time*. New York: Penguin Press.

***In-text citations***

* (Grazer and Fishman 2015)

* (Smith 2016)

For more examples, see [15.40–45](http://www.chicagomanualofstyle.org/book/ed17/part3/ch15/psec040.html) in The Chicago Manual of Style.

### Chapter or other part of an edited book

In the reference list, include the page range for the chapter or part.

***Reference list entry***

* Thoreau, H. D. 2016. Walking. Pp. 167–195 *In* J. D’Agata, ed. The Making of the American Essay. Graywolf Press, Minneapolis.

***In-text citation***

* (Thoreau 2016)

In some cases, you may want to cite the collection/book as a whole instead.

***Reference list entry***

D’Agata, J., ed. 2016. The Making of the American Essay. Graywolf Press, Minneapolis.

***In-text citation***

* (D’Agata 2016)

### Journal Article

In the reference list, include the page range for the whole article. For articles consulted online, include a URL or the name of the database in the reference list entry. Many journal articles list a DOI (Digital Object Identifier). A DOI forms a permanent URL that begins https://doi.org/. This URL is preferable to the URL that appears in your browser’s address bar.

***Reference list entries*** (in alphabetical order)

* LaSalle, P. 2017. Conundrum: A Story about Reading. New England Review 38:95–109.

* Satterfield, S. 2016. Livy and the Pax Deum. Classical Philology 111:165–76.

***In-text citations***

* (LaSalle 2017)

* (Satterfield 2016)

### Journal Article with many authors

Journal articles often list many authors, especially in the sciences. If there are three or more authors, list up to ten in the reference list; in the text, list only the first, followed by et al. (“and others”). For more than ten authors, list the first six in the reference list, followed by et al.

***Reference list entry***

* Alroy, J., M. Aberhan, D. J. Bottjer, M. Foote, F. T. Fuersich, P. J. Harries, et al. (2008). Phanerozoic trends in the global diversity of marine invertebrates. Science, 321:97–100. doi: 10.1126/science.1156963.

* Bay, R. A., N. Rose, R. Barrett, L. Bernatchez, C. K. Ghalambor, J. R. Lasky, R. B. Brem, S. R. Palumbi, and P. Ralph. 2017. Predicting Responses to Contemporary Environmental Change Using Evolutionary Response Architectures. American Naturalist 189:463–73.  doi: 10.1086/691233.

* Keng, S.-H., C.-H. Lin, and P. F. Orazem. 2017. Expanding College Access in Taiwan, 1978–2014: Effects on Graduate Quality and Income Inequality. Journal of Human Capital 11:1–34. doi: 10.1086/690235.

***In-text citation***

* (Alroy et al. 2008)

* (Bay et al. 2017)

* (Keng et al. 2017)

# SuperPlotsOfData
 
SuperPlotsofData uses modern data visualization strategies for transparent presentation of data. The key features are:

* identification of replicas and their statistical summary (mean of median) by colorblind friendly colors and/or symbols
* communicate experimental design (number of replicas, paired or non-paired)
* enables raincloud plots for plotting the data and distribution
* makes a qantitative comparison between conditions by calculating the effect size (and a p-value)

Both technical replicates (repeated measurements of the same biological sample) and biological replicates (repeated measurements of different biological samples) will contribute variability to experimental data. Classical plots do not distinguish between these sources of variability and may therefore incorrectly convey the precision of the measurement. A SuperPlot shows the data of all measurements and distuingishes between biological replicates. This data visualization strategy was first published in a [preprint](https://arxiv.org/abs/1911.03509) and is now published in a peer reviewed paper by [Lord et al (2020)](https://doi.org/10.1083/jcb.202001064).
The SuperPlotsOfData Shiny app combines the idea of SuperPlots with the philosophy of [PlotsOfData app](https://huygens.science.uva.nl/PlotsOfData/) which was developped for plotting the raw data (instead of a summary) to improves transparency and interpretation. Summary statistics (mean, median) are available for the individual (biological replicates). The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha). More details are reported in [a preprint](https://doi.org/10.1101/2020.09.01.276881) (doi: 10.1101/2020.09.01.276881)

### Running the App

The app is available online: [https://huygens.science.uva.nl/SuperPlotsOfData/](https://huygens.science.uva.nl/SuperPlotsOfData/) or [https://amsterdamstudygroup.shinyapps.io/SuperPlotsOfData/](https://amsterdamstudygroup.shinyapps.io/SuperPlotsOfData/)

But you can also run it offline in R/Rstudio. To run the app locally, you'll need the following packages: shiny, ggplot2, dplyr, tidyr, readr, magrittr, ggbeeswarm, readxl, DT, RCurl, broom. To download and install all the packages at once use in Rstudio:

```
install.packages("shiny", "tidyverse", "DT", "ggbeeswarm", "RCurl", "broom")

library(shiny)
library(tidyverse)
library(ggbeeswarm)
library(DT)
library(RCurl)
library(broom)
```

To run the app from the Github repo, type in the command line (in R or Rstudio):

`shiny::runGitHub('SuperPlotsOfData', 'JoachimGoedhart')`

Or download it to use it offline:

- download all the files from the Github repository.
- Run RStudio and load the `app.R` file from the repository
- Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.


### Example output

Standard output generated with the example data:

![Example SuperPlot](https://github.com/JoachimGoedhart/SuperPlotsOfData/blob/master/SuperPlotsOfData.png?raw=true "image")

### Credits

SuperPlotsOfData is build on the app: "PlotsOfData - A web app for visualizing data together with their summaries" - doi: [10.1371/journal.pbio.3000202](https://doi.org/10.1371/journal.pbio.3000202)

The Superplot as a means of communicating about replicates is published by [Lord et al (2020)](https://doi.org/10.1083/jcb.202001064).

The colorblind safe palettes were developed by [Paul Tol](https://personal.sron.nl/~pault/) and [Okabe & Ito](https://jfly.uni-koeln.de/color/). You can read more about these palettes [in this blog](https://thenode.biologists.com/data-visualization-with-flying-colors/research/)</p>

### Contact

SuperPlotsOfData is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))

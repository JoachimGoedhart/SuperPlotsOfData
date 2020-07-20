# SuperPlotsOfData
 
SuperPlotsofData uses modern data visualization techniques for the quantitative display of data. The key features are:
* identification of replicas and their statistical summary (mean of median) by colorblind friendly colors
* communicates experimental design (number of replicas, paired or non-paired)
* enables raincloud plots for plotting the data and distribution
* makes a qantitative comparison between conditions by calculating the effect size (and a p-value)

Both technical replicates (repeated measurements of the same biological sample) and biological replicates (repeated measurements of different biological samples) will contribute variability to experimental data. Classical plots do not distinguish between these sources of variability and may therefore incorrectly convey the precision of the measurement. A SuperPlot shows the data of all measurements and distuingishes between biological replicates. This data visualization strategy was first published in a [preprint](https://arxiv.org/abs/1911.03509) and is now published in a peer reviewed paper by [Lord et al (2020)](https://doi.org/10.1083/jcb.202001064).
The SuperPlotsOfData Shiny app combines the idea of SuperPlots with the philosophy of [PlotsOfData app](https://huygens.science.uva.nl/PlotsOfData/) which was developped for plotting the raw data (instead of a summary) to improves transparency and interpretation. Summary statistics (mean, median) are available for the individual (biological replicates). The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha).

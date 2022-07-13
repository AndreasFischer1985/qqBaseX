# qqBaseX
The qqBaseX package provides basic functions for assisting all steps of analyzing qualitative and quantitative data. It was written to extend R's default packages without introducing dependencies on other packages. 

Besides enhanced variants of standard plots and methods (e.g., *bp()* or *dotplot()*), the qqBaseX package offers a number of convenience functions - like *saveDevs()*, to save open graphics devices in high resolution in various formats, or *cols()*, to easily create color series. In addition, the package includes many functions for exploring datasets (e.g., *plotDF()* for data.frames or *plotMAT()* for matrices), functions for visualizing variables (e.g., *spiderplot()* or *flowerplot()*), functions for annotating plots (e.g., *boxedText()*) and functions for visualizing statistical analyses (e.g., *plotLM()* for linear models). The package also includes model-agnostic functions for quantifying the influence of individual variables on predictions (for example, *af.sensitivity()* and *delta()*). In addition, the package provides functions for sourcing data via web-scraping (e.g., *scrapeHTML()* or *extractTable()*) and for preprocessing data (e.g., *prepareDF()* for quantitative data analysis or *vecToTDM()* for qualitative data analysis).

The qqBaseX package can be installed using the following commands in R:

```
install.packages("devtools");
devtools::install_github("AndreasFischer1985/qqBaseX");
```

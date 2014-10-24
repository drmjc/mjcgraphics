mjcgraphics
===========

An R package of various graphics plots and wrappers. Mostly using base graphics for bioinformatics and microarray data.

INSTALLATION
------------
    # install Bioconductor if you haven't already done so:
    source("http://bioconductor.org/biocLite.R")
    biocLite()
    
    # install required CRAN/Bioconductor packages
    library(BiocInstaller)
    biocLite(c("limma", "lumi"))
    install.packages(c("gplots", "gtools", "RColorBrewer", "devtools"))
    
    library(devtools)
    install_github("drmjc/excelIO")
    install_github("drmjc/mjcbase")
    install_github("drmjc/mjcgraphics")
    
Popular Functions
-----------------
1) Convenient wrappers around png(), jpeg(), pdf() to create standard sized images
    
    ?plotting.devices
    ?png.VGA, ?pdf.A4, etc...

2) Correspondence-at-the-top (CAT) plots

    ?catplot
    ?catplot.vs.random

3) plot all rows (as lines) or columns (as density plots) of a matrix
    
    ?plot.matrix
    ?plot.density.matrix

4) convenient grids

    ?hgrid, ?vgrid, ?grid.percentiles

5) Combination of a sorted scatter plot, and boxplot
    
    ?comboplot_scatter_boxplot

6) Errorbar plots, and staistical significance

    ?errorbarplot, ?pvalue.stars

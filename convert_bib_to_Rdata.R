# set working directory
setwd("~/DriveUEMEncrypt/casa/uem/Doutorado/Revisão/SAF_bibliometric_analysis/bib_analysis_bibliometrix_R")

# to have better control of the charts generation we run our on commands
# they are documented on the bibliometrix manual: https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
file <- ("complete_dataset.bib");

# Import and Convert bibliographic file
M <- convert2df(file, dbsource = "wos", format = "bibtex");

# save dataframe
save(M, file = "complete_dataset.RData")
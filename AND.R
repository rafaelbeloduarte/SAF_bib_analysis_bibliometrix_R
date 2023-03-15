# Program to perform author name disambiguation
# Copyright (C) 2022  Rafael Belo Duarte
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# contact me at rafaelbeloduarte@pm.me

# to install refsplitr:
# install.packages("devtools")
# devtools::install_github("ropensci/refsplitr")

library(refsplitr)

# for a shiny UI for bibliometrix package run
# biblioshiny()

# set working directory
setwd("~/DriveUEMEncrypt/casa/uem/Doutorado/Revisão/SAF_bibliometric_analysis/bib_analysis_bibliometrix_R")

# import refs
refs <- references_read(data = "WOS_dataset/txt", 
                        dir=TRUE,
                        include_all = TRUE)

# save references
write.csv(refs,"WOS_dataset/output_refsplitr/refs.csv")
# load processed references
# write.csv(refs,"WOS_dataset/output_refsplitr/refs.csv")

# clean refs
refs_clean <- authors_clean(refs)

# save cleaned results
write.csv(refs_clean$prelim,"WOS_dataset/output_refsplitr/refs_clean_prelim.csv")
write.csv(refs_clean$review,"WOS_dataset/output_refsplitr/refs_clean_review.csv")

# accept the results of author disambiguation
refs_refined <- authors_refine(refs_clean$review, 
                               refs_clean$prelim)

write.csv(refs_refined,"complete_dataset_ANDED_refined.csv")

georef <-authors_georef(data=refs_refined, address_column = "address")

# plot_addresses_country <- plot_addresses_country(georef$addresses)
plot_addresses_points <- plot_addresses_points(georef$addresses)
# plot_addresses_points
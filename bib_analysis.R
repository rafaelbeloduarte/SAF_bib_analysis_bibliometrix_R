## ---- licence

# Program to perform bibliometric analysis
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

## ---- bib_analysis_results
library(bibliometrix)

# set working directory
setwd("~/DriveUEMEncrypt/casa/uem/Doutorado/Revisão/SAF_bibliometric_analysis/SAF_bib_analysis_bibliometrix_R")

dataset_ANDED <- readRDS("complete_dataset_ANDED.rds")
results <- biblioAnalysis(dataset_ANDED, sep = ";")
# saveRDS(dataset_ANDED, file = "dataset_ANDED_for_biblioshiny.rds")
attributes(results$Documents)
n_articles <- results$Documents[[1]] + results$Documents[[3]]
n_proceedings <- results$Documents[[4]] + results$Documents[[10]]
n_reviews <- results$Documents[[11]] + results$Documents[[12]]
n_news <- results$Documents[[9]]
n_editorial <- results$Documents[[6]]
n_meeting_abs <- results$Documents[[8]]
n_corrections <- results$Documents[[5]]
n_data_papers <- results$Documents[[2]]
n_letters <- results$Documents[[7]]
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
AUperPaper_mode <- Mode(results$nAUperPaper)

## ---- missing_data
library(knitr)
library(kableExtra)
misssing_data <- missingData(dataset_ANDED)
## ---- missing_data_table
kable(misssing_data$mandatoryTags,  caption = "Missing data. \\label{tab:missing_data}", booktabs = TRUE,
      col.names = c("Tag",	"Description",	"Missing Counts",	"Missing %", "Status")) %>%
  kable_styling(font_size = 7, latex_options = "scale_down")

## ---- summary
S <- summary(object = results, k = 10, pause = FALSE)

## ---- plot_summary
plots_bib <- plot(x = results, k = 10, pause = FALSE)

## ---- barplot_most_prod_countries
par(mar=c(5,10,1,1))
barplot(as.numeric(S$MostProdCountries$Articles), names.arg = S$MostProdCountries$Country, horiz = TRUE, las = 1, xlab = "Number of publications.")

## ---- plot_int_collab
par(mar=c(5,10,1,1))
barplot(as.numeric(S$MostProdCountries$MCP_Ratio), names.arg = S$MostProdCountries$Country, horiz = TRUE, las = 1, xlab = "Multiple to single country publication ratio.")

## ---- main_info_table
library(knitr)
library(kableExtra)
kable(S$MainInformationDF,  format = "latex", caption = "Main information. \\label{tab:main_info}", booktabs = TRUE) %>%
  kable_styling(font_size = 7)

## ---- most_relevant_sources
# get the sources from summary
most_rel_sources <- S$MostRelSources
# filter out some characters
most_rel_sources$`Sources       ` <- gsub("&", "000", most_rel_sources$`Sources       `)
most_rel_sources$`Sources       ` <- gsub("  ", "", most_rel_sources$`Sources       `)
most_rel_sources$`Sources       ` <- gsub("[[:punct:]]", " ", most_rel_sources$`Sources       `)
most_rel_sources$`Sources       ` <- gsub("000", "&", most_rel_sources$`Sources       `)
most_rel_sources$`Sources       ` <- gsub("[\r\n]", "", most_rel_sources$`Sources       `)
most_rel_sources$`Sources       ` <- gsub("  ", " ", most_rel_sources$`Sources       `)

most_rel_sources_code <- gsub("[[:punct:]]", " ", most_rel_sources$`Sources       `)
most_rel_sources_code <- gsub(" ", "", most_rel_sources_code)
sources_code <- gsub("[[:punct:]]", "", dataset_ANDED$SO)
sources_code <- gsub(" ", "", sources_code)

# search in dataset_ANDED$SO for the most relevant sources ISSNs
most_rel_sources_issn <- data.frame()
for (i in 1:length(most_rel_sources_code)) {
  indices <- which(sources_code == most_rel_sources_code[[i]])
  # get the subset that matches and omit NA cells
  data_subset <- na.omit(dataset_ANDED$SN[indices])
  # take the mode of the returned values to avoid getting empty rows
  most_rel_sources_issn <- rbind(most_rel_sources_issn, Mode(data_subset))
}
colnames(most_rel_sources_issn) <- "ISSN"
most_rel_sources$ISSN <- most_rel_sources_issn$ISSN

# get the impact factors
library(readxl)
latestJCRlist2022 <- read_excel("latestJCRlist2022.xlsx")
# search the journals indices in the JCR list by ISSN
most_rel_sources_impact <- data.frame()
for (i in 1:length(most_rel_sources$ISSN)) {
  indices <- which(latestJCRlist2022$issn == most_rel_sources$ISSN[[i]])
  # get the subset that matches and omit NA cells
  data_subset <- na.omit(latestJCRlist2022$if_2022[indices])
  # take the mode of the returned values to avoid getting empty rows
  if (length(indices) == 0) {
    most_rel_sources_impact <- rbind(most_rel_sources_impact, NA)
  }
  else {
    most_rel_sources_impact <- rbind(most_rel_sources_impact, data_subset)
  }
}
colnames(most_rel_sources_impact) <- "Impact Factor"
most_rel_sources$impact_factor <- most_rel_sources_impact$`Impact Factor`

library(rcrossref)
crossref_data <- cr_journals(issn = most_rel_sources_issn$ISSN)
most_rel_sources$Publisher <- crossref_data$data$publisher

# calculate the % of SAF articles in relation to the total number of DOIs of the source
most_rel_sources$perc_of_total_dois <- 100 * as.integer(most_rel_sources$Articles)/crossref_data$data$total_dois

# reorder the table
colnames(most_rel_sources) <- c("Sources", "Documents", "ISSN", "Impact Factor 2022", "Publisher", "% of total DOIs")
most_rel_sources <- most_rel_sources[, c("Sources", "Documents", "% of total DOIs", "Impact Factor 2022", "ISSN", "Publisher")]

kable(most_rel_sources,  format = "latex", row.names = FALSE, booktabs = TRUE, escape = TRUE, linesep = "", caption = "Most relevant sources by number of documents. \\label{tab:most_rel_sources}", table.envir = "table*")  %>%
  kable_styling(font_size = 7, latex_options = "scale_down")

## ---- most_prod_authors
library(tibble)
library(dplyr)
listAU <- (strsplit(dataset_ANDED$AU, ";"))
nAU <- lengths(listAU)
fracAU <- rep(1/nAU,nAU)
most_prod_authors <- tibble(Author=unlist(listAU), fracAU=fracAU) %>% 
  group_by(.data$Author) %>% 
  summarize(
    Articles = n(),
    AuthorFrac = sum(.data$fracAU)
  ) %>% 
  arrange(desc(.data$Articles)) %>% as.data.frame()
names(most_prod_authors)=c("Authors","Articles","Articles Fractionalized")
most_prod_authors[1:15,]
#print(S$MostProdAuthors)

# function to search for authors information
search_author_info <- function(names_list, df) {
  authors_info <- data.frame()
  for (i in 1:length(names_list)) {
    indices <- which(df$author_name == names_list[[i]])
    data_subset <- df[indices, ]
    # if orcid id exists use those lines
    if (TRUE %in% complete.cases(data_subset[,"OI"])) {
      data_subset <- data_subset[complete.cases(data_subset[,"OI"]),]
    }
    authors_info <- rbind(authors_info, data_subset[1,])
  }
  return(authors_info)
}

library(readr)

AND_refined <- read.csv("~/DriveUEMEncrypt/casa/uem/Doutorado/Revisão/SAF_bibliometric_analysis/SAF_bib_analysis_bibliometrix_R/AND_refined.csv")
# used gsub to clean the names list and make them equal between datasets
# AND_refined$author_name <- gsub("\\.", "", AND_refined$author_name)
# remove some empty cells
AND_refined <- AND_refined[complete.cases(AND_refined[,c("country", "university")]),]

most_prod_authors_info <- search_author_info(most_prod_authors$Author, AND_refined)

most_prod_authors$Affiliation <- most_prod_authors_info$university
most_prod_authors$Country <- most_prod_authors_info$country
most_prod_authors$"Orcid ID" <- most_prod_authors_info$OI
most_prod_authors[1:10,]

## ---- most_prod_authors_table
kable(most_prod_authors[1:10,],  format = "latex", caption = "Most productive authors. \\label{tab:most_prod_authors}", booktabs = TRUE, table.envir = "table*", digits = 1) %>%
  kable_styling(font_size = 7, latex_options = "scale_down")

## ---- h_index
library(dplyr)
h <- Hindex(dataset_ANDED, field = "author", sep = ";", years=Inf)$H %>% 
  arrange(desc(h_index))
h_table <- h[,c("Element", "h_index", "PY_start", "TC", "NP")]
AND_refined_no_comma <- AND_refined
AND_refined_no_comma$author_name <- gsub(",", "", AND_refined_no_comma$author_name)

# df <- filter(dataset_ANDED, grepl("Zhang, Xiangwen", AU, fixed = TRUE))

h_index_authors_info <- search_author_info(h_table$Element, AND_refined_no_comma)
h_table$Affiliation <- h_index_authors_info$university
h_table$Country <- h_index_authors_info$country
h_table$"Orcid ID" <- h_index_authors_info$OI
for (i in 1:10) {
  df <- filter(dataset_ANDED, grepl(h$Element[i], gsub(",", "", AU), fixed = TRUE))
  h_table$"Most freq. keywords"[i] <-  toString(rownames(tableTag(df, Tag = "DE", sep = ";",
                                                                                synonyms = NULL, remove.terms = NULL)[0:5]))
}
h_table[0:10,]

## ---- h_index_table
kable(h_table[0:10,], format = "latex", caption = "Authors' local impact in order of h index. \\label{tab:h_index}", booktabs = TRUE, table.envir = "table*", col.names = c("Author", "H index", "First publication", "Total citations", "Publications", "Affilliation", "Country", "Orcid ID", "Most freq. keywords")) %>%
  kable_styling(font_size = 7, latex_options = "scale_down") %>%
  column_spec(9, width = "25em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "5em")


## ---- slice_2022
dataset_ANDED_2022 <- timeslice(dataset_ANDED, breaks = c(2022))
results_2022 <- biblioAnalysis(dataset_ANDED_2022$`(1994,2022]`, sep = ";")
S_2022 <- summary(object = results_2022, k = 10, pause = FALSE)
## ---- slice_2012_2022
dataset_ANDED_2012_2022 <- timeslice(dataset_ANDED, breaks = c(2012,2022))
results_2012_2022 <- biblioAnalysis(dataset_ANDED_2012_2022$`(2012,2022]`, sep = ";")
S_2012_2022 <- summary(object = results_2012_2022, k = 10, pause = FALSE)

## ---- slice_2016_2020
library(dplyr)
dataset_ANDED_2016_2020 <- timeslice(dataset_ANDED, breaks = c(2015, 2020))
results_2016_2020 <- biblioAnalysis(dataset_ANDED_2016_2020$'(2015,2020]', sep = ";")
results_2020_2023 <- biblioAnalysis(dataset_ANDED_2016_2020$'(2020,2023]', sep = ";")
total_reviews <- results[["Documents"]][["REVIEW                         "]]
reviews_2016_2020 <- results_2016_2020[["Documents"]][["REVIEW                         "]]
reviews_2020_2023 <- results_2020_2023[["Documents"]][["REVIEW                  "]]

## ---- plot_review_age_vs_citation
library(dplyr)
reviews_dataset <- filter(dataset_ANDED, DT == "REVIEW")
article_dataset <- filter(dataset_ANDED, DT == "ARTICLE")
results_review <- biblioAnalysis(reviews_dataset, sep = ";")
results_article <- biblioAnalysis(reviews_dataset, sep = ";")
review_age <- 2024 - reviews_dataset$PY
article_age <- 2024 - article_dataset$PY
review_citations_per_year <- reviews_dataset$TC/review_age
article_citations_per_year <- article_dataset$TC/article_age
plot(article_age-1,article_citations_per_year, type = "p", lty = 0, pch = 4, col = "gray", xlab = "Age in years", ylab = "Total citations/age", ylim = c(0,max(review_citations_per_year, article_citations_per_year)))
lines(review_age-1, review_citations_per_year, type = "p", lty = 0)
legend(x = 1, y = max(review_citations_per_year, article_citations_per_year), c("Articles", "Reviews"), pch = c(4,1), col = c("gray", "black"))

# boxplot(article_citations_per_year, review_citations_per_year, names = c("Articles", "Reviews"),  ylab = "Total citations/age")

## ---- bib_analysis_results_before_AND
dataset_NOT_ANDED <- convert2df("complete_dataset.bib", dbsource = "wos", format = "bibtex")
results_NOT_ANDED <- biblioAnalysis(dataset_NOT_ANDED, sep = ";")

## ---- author_collaboration_network
M_AU <- metaTagExtraction(dataset_ANDED, Field = "AU", sep = ";");
NetMatrix_AU <- biblioNetwork(M_AU, analysis = "collaboration", network = "authors", sep = ";");
par(mar=c(0,2,0,2))
net_AU=networkPlot(NetMatrix_AU,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=FALSE,
                   label = TRUE,
                   labelsize = 3,
                   label.color = TRUE,
                   cluster = "edge_betweenness",
                   community.repulsion = 0.0001,
                   label.cex = TRUE,
                   remove.isolates = FALSE,
                   edgesize = 2,
                   alpha = 0.6,
                   halo = FALSE);

# net2VOSviewer(net_AU);

## ---- filtered_terms
removed_terms = c("2", "assessment", "use", "analysis", 
                  "performance", "alternative", "green",
                  "process", "environmental")
synonyms_list=c("a.f.;alternative fuels;alternative fuel;biofuels;
                bio-fuels;biofuel;bio-fuel;bio-oil;
                advanced biofuels",
                "saf;sustainable aviation fuel;
                sustainable aviation fuel (saf);
                sustainable aviation fuels",
                "bio-jet fuel;
                jet biofuel;biojet fuel;bio jet fuel;
                bio-jet fuels;bio jet fuel ;bio-jet;
                bio-kerosene;biokerosene;
                aviation biofuel;aviation biofuels;
                bio-aviation fuel; aviation biofuels ;biojet",
                "alternative jet fuel;alternative jet fuels;
                renewable jet fuel;renewable aviation fuel",
                "aviation fuels;aviation fuel;jet fuel;
                aviation fuels;jet;kerosene;jet a-1",
                "sustainability;sustainable development;
                sustainable;environmental impact",
                "emissions;emission;aviation emissions;
                aircraft emissions;co2 emissions;
                greenhouse gas;greenhouse gas emissions;
                ghg;ghg emissions;air pollution;",
                "diesel;biodiesel;bio-diesel;
                green diesel;diesel engine",
                "lca;life cycle assessment;life-cycle assessment;life cycle",
                "ethanol;bioethanol",
                "aviation;sustainable aviation;
                aircraft;air transport",
                "tec. econ. analysis;
                techno-economic analysis;techno-economic",
                "deoxygenation;hydrodeoxygenation",
                "HTL;hydrothermal liquefaction",
                "energy;renewable energy;bioenergy",
                "pyrolysis;fast pyrolysis;
                catalytic pyrolysis",
                "catalysis;catalysts;catalyst",
                "drop-in;drop-in biofuels;drop-in fuel",
                "hydroprocessing;hydrogenation;
                hydrocracking;hydrotreating",
                "isomerization;hydroisomerization",
                "efficiency;energy efficiency",
                "combustion;combustion characteristics",
                "fts;fischer-tropsch"
                );

## ---- titles_keywords_co-occurrence_network
title_terms <- termExtraction(
                dataset_ANDED,
                Field = "TI",
                ngrams = 1,
                stemming = FALSE,
                language = "english",
                remove.numbers = TRUE,
                remove.terms = removed_terms,
                keep.terms = NULL,
                synonyms = synonyms_list,
                verbose = TRUE
)
NetMatrix_TI <- biblioNetwork(title_terms, analysis = "co-occurrences",
                              network = "titles", sep = ";",
                              synonyms = synonyms_list,
                              remove.terms = removed_terms);

# net2VOSviewer(net_kw);
par(mar=c(0,0,0,0))
net_kw=networkPlot(NetMatrix_TI,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 15,
                   size.cex = TRUE,
                   remove.multiple=TRUE,
                   labelsize = 1.1,
                   label.cex = FALSE,
                   cluster = "none",
                   community.repulsion = 0.1,
                   remove.isolates = TRUE,
                   noloops = TRUE,
                   edgesize = 0.5,
                   # edges.min = 50,
                   alpha = 0.6,
                   halo = FALSE,
                   curved = FALSE);

## ---- authors_keywords_co-occurrence_network
NetMatrix_kw <- biblioNetwork(dataset_ANDED, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";",
                              synonyms = synonyms_list,
                              remove.terms = removed_terms);

# net2VOSviewer(net_kw);
par(mar=c(0,0,0,0))
net_kw=networkPlot(NetMatrix_kw,
                   n = 60,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=TRUE,
                   labelsize = 0.6,
                   label.cex = FALSE,
                   label.color = TRUE,
                   cluster = "fast_greedy",
                   community.repulsion = 0.1,
                   remove.isolates = TRUE,
                   noloops = TRUE,
                   edgesize = 1,
                   alpha = 0.6,
                   halo = FALSE,
                   curved = FALSE);

## ---- word_cloud
library(wordcloud2)
library(webshot2)
library("htmlwidgets")
# webshot::install_phantomjs()

Tab <- tableTag(dataset_ANDED, Tag = "TI", sep = ";",
                synonyms = NULL, remove.terms = removed_terms);
Tab <- log(Tab);
wc <- wordcloud2(Tab, size = 1, minSize = 0, gridSize =  10,
                   fontFamily = 'Liberation Serif', fontWeight = 'bold',
                   color = 'random-dark', backgroundColor = "white",
                   minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
                   rotateRatio = 0, shape = 'circle', ellipticity = 0.8,
                   widgetsize = NULL, figPath = NULL, hoverFunction = NULL);
Sys.setenv("OPENSSL_CONF"="/dev/null")
saveWidget(wc, "figure/wc.html"  , selfcontained = F)
webshot(
  url = "figure/wc.html",
  file = "figure/wc.png",
  delay = 5,
  selector = "#canvas",
  vwidth = 1920,
  vheight = 1080
)

## ---- most_locally_cited_papers
library(knitr)
library(kableExtra)
library(rcrossref)
local_citations <- localCitations(dataset_ANDED, fast.search = FALSE, sep = ";", verbose = FALSE)
local_citations_crossref_data <- cr_works(dois = local_citations$Papers$DOI[1:10])
citation_ratio <- local_citations$Papers$LCS/local_citations$Papers$GCS
local_citations_df <- data.frame(Titles=local_citations_crossref_data$data$title, Year=as.numeric(local_citations$Papers$Year[1:10]), Local_Citations=as.numeric(local_citations$Papers$LCS[1:10]), Global_Citations=as.numeric(local_citations$Papers$GCS[1:10]), LC_GC_ratio = citation_ratio[1:10])
kable(local_citations_df[1:10,], format = "latex", caption = "Most locally cited papers. \\label{tab:most_locally_cited}", booktabs = TRUE, table.envir = "table*", col.names = c("Title (DOIs are provided as hyperlinks)", "Year", "Local citations (LC)", "Global citations (GC)", "LC/GC ratio")) %>%
  kable_styling(font_size = 7, latex_options = "scale_down") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(1, link = local_citations_crossref_data$data$url)

## ---- most_locally_cited_papers_analysis
library(dplyr)
most_cited <- dplyr::filter(dataset_ANDED, DI == local_citations[["Papers"]][["DOI"]][1])
for (i in 2:10){
  print(i)
  most_cited[nrow(most_cited) + 1,] <- dplyr::filter(dataset_ANDED, DI == local_citations[["Papers"]][["DOI"]][i])
}

NetMatrix_most_cited <- biblioNetwork(most_cited, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";",
                              synonyms = NULL,
                              remove.terms = NULL);
net_most_cited=networkPlot(NetMatrix_most_cited,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=TRUE,
                   labelsize = 0.7,
                   label.cex = FALSE,
                   label.color = TRUE,
                   cluster = "walktrap",
                   community.repulsion = 0.01,
                   remove.isolates = TRUE,
                   noloops = TRUE,
                   edgesize = 1,
                   alpha = 0.6,
                   halo = FALSE,
                   curved = FALSE);

M_AU_most_cited <- metaTagExtraction(most_cited, Field = "AU", sep = ";");
NetMatrix_AU_most_cited <- biblioNetwork(M_AU_most_cited, analysis = "collaboration", network = "authors", sep = ";");
net_AU_most_cited=networkPlot(NetMatrix_AU_most_cited,
                   n = 1000,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 1,
                   size.cex = F,
                   remove.multiple=FALSE,
                   label = TRUE,
                   labelsize = 0.8,
                   label.color = TRUE,
                   cluster = "edge_betweenness",
                   community.repulsion = 0.01,
                   label.cex = TRUE,
                   remove.isolates = TRUE,
                   edgesize = 1,
                   alpha = 0.7,
                   halo = FALSE);

## ---- keyword_growth
topKW=KeywordGrowth(dataset_ANDED, Tag = "ID", sep = ";", top=10, cdf=TRUE)
topKW
# Plotting results
# install.packages("reshape2")
library(reshape2)
library(ggplot2)
DF=melt(topKW, id='Year')
localCitations
ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line()

## ---- country_collaboration_network
M_CO <- metaTagExtraction(dataset_ANDED, Field = "AU_CO", sep = ";");
NetMatrix_CO <- biblioNetwork(M_CO, analysis = "collaboration", network = "countries", sep = ";");
par(mar=c(0,0,0,0))
net_CO=networkPlot(NetMatrix_CO,
                n = 30,
                normalize = "association",
                Title = NULL,
                type = "fruchterman",
                size = 10,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize = 6,
                cluster = "edge_betweenness",
                community.repulsion = 0.05,
                label.cex = TRUE,
                remove.isolates = FALSE,
                edgesize = 5,
                edges.min = 5,
                alpha = 0.5,
                curved = FALSE);
# net2VOSviewer(net_CO);

## ---- country_collaboration_network_interactive
# thanks to massimoaria for sharing the igraph2Vis function:
# https://github.com/massimoaria/bibliometrix/issues/69
library(visNetwork)
igraph2vis<-function(g,curved,labelsize,opacity,type,shape){
  LABEL <- igraph::V(g)$name
  LABEL[igraph::V(g)$labelsize==0] <- ""
  vn <- toVisNetworkData(g)
  
  vn$nodes$label <- LABEL
  vn$edges$num <- 1
  vn$edges$dashes <- FALSE
  vn$edges$dashes[vn$edges$lty==2] <- TRUE
  
  ## opacity
  vn$nodes$color <- adjustcolor(vn$nodes$color,alpha=min(c(opacity+0.2,1)))
  vn$edges$color <- adjustcolor(vn$edges$color,alpha=opacity)
  
  ## removing multiple edges
  vn$edges <- unique(vn$edges)
  
  ## labelsize
  scalemin <- 20
  scalemax <- 150
  Min <- min(vn$nodes$font.size)
  Max <- max(vn$nodes$font.size)
  if (Max>Min){
    size <- (vn$nodes$font.size-Min)/(Max-Min)*10*labelsize+10
  } else {size <- 10*labelsize}
  size[size<scalemin] <- scalemin
  size[size>scalemax] <- scalemax
  vn$nodes$font.size <- size
  l <- type
  
  ### TO ADD SHAPE AND FONT COLOR OPTIONS
  
  VIS <- visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>%
    visNodes(shape=shape, font=list(color="black")) %>%
    visIgraphLayout(layout = l) %>%
    visEdges(smooth = curved) %>%
    visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
    visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE)
  return(list(VIS=VIS,vn=vn, type=type, l=l, curved=curved))
  
}
country_net_igraph <- igraph2vis(net_CO$graph, curved = TRUE, labelsize = 5, opacity = 0.6, type = "layout_with_fr", shape = "circle")
Sys.setenv("OPENSSL_CONF"="/dev/null")
library(webshot2)
library("htmlwidgets")
saveWidget(country_net_igraph$VIS, "figure/country_net_igraph.html"  , selfcontained = F)
webshot(
  url = "figure/country_net_igraph.html",
  file = "figure/country_net_igraph.png",
  delay = 5,
  selector = "#maindivhtmlwidget-3ce7d6739b972c49d361",
  vwidth = 4000,
  vheight = 2000)

## ---- countries_network_statistics
netstat_countries <- networkStat(NetMatrix_CO, stat = "all", type = "degree")
summary(netstat_countries)
kable(summary(netstat_countries),  caption = "Countries network statistics.", booktabs = TRUE) %>%
  kable_styling(font_size = 7)

## ---- collaboration_network_between_institutions
M_AFF <- metaTagExtraction(dataset_ANDED, Field = "AU1_UN", sep = ";");
NetMatrix_AFF <- biblioNetwork(M_AFF, analysis = "collaboration", network = "universities", sep = ";");
net_AFF=networkPlot(NetMatrix_AFF,
                n = 50,
                normalize = "association",
                Title = NULL,
                type = "auto",
                size = 10,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize=5,
                cluster = "edge_betweenness",
                community.repulsion = 0.01,
                label.cex = TRUE,
                remove.isolates = FALSE,
                edgesize = 20,
                alpha = 0.7);

## ---- historical_citation_network
histResults <- histNetwork(dataset_ANDED, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, size = 10)

## ---- reference_publication_year_spectrocopy
ref_spec <- rpys(dataset_ANDED, sep = ";", timespan = c(1990,2022), graph = FALSE)
plot(ref_spec$rpysTable$Year,ref_spec$rpysTable$Citations, type = "l", lty = 0, xlab = "Year", ylab = "Citations")
lines(ref_spec$rpysTable$Year,ref_spec$rpysTable$Citations, type = "l", lty = 2)
lines(ref_spec$rpysTable$Year,ref_spec$rpysTable$diffMedian5, type = "o", lty = 1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
legend(ref_spec$rpysTable$Year[[1]], y = max(ref_spec$rpysTable$Citations), c("Number of cited references", "Deviation from the 5 year median"), lty = c(2,1), bg = "white")

## ---- historiograph
histResults <- histNetwork(dataset_ANDED, min.citations = 1, sep = ";", network = TRUE, verbose = TRUE);
histPlot(
  histResults,
  n = 20,
  size = 5,
  labelsize = 5,
  title_as_label = FALSE,
  verbose = TRUE
);

## ---- co-citation_analysis
NetMatrix_coc <- biblioNetwork(dataset_ANDED, analysis = "co-citation", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_coc, n = 30, Title = "Co-Citation Network", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                remove.multiple=FALSE, labelsize=2,edgesize = 5, label.cex = TRUE, community.repulsion = 0.01);

## ---- bib_coupling
NetMatrix_bib_coup <- biblioNetwork (dataset_ANDED, analysis = "coupling", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_bib_coup, n = 50, Title = "Bibliographic coupling", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                    remove.multiple=FALSE, labelsize=2,edgesize = 1, label.cex = TRUE, community.repulsion = 0.01);

## ---- conceptual_structure
CS <- conceptualStructure(dataset_ANDED, field="DE", method="CA", 
                          stemming=TRUE, minDegree=10, k.max = 5, synonyms = synonyms_list);

years=c(2005,2010,2015)
nexus <- thematicEvolution(dataset_ANDED,field="DE", years=years, n=100,minFreq=2)
plotThematicEvolution(nexus$Nodes,nexus$Edges)

## ---- coupling_map
# It performs a coupling network analysis and plots 
# community detection results on a bi-dimensional
# map (Coupling Map).
res <- couplingMap(dataset_ANDED, analysis = "authors", field = "CR", n = 250, impact.measure="local",
                   minfreq = 3, size = 0.5, repel = TRUE)
plot(res$map)

## ---- historical_citation_network
histResults <- histNetwork(dataset_ANDED, sep = ";", network = TRUE)
# Plot a historical co-citation network
net <- histPlot(histResults, size = 5)

## ---- conceptual_structure
CS <- conceptualStructure(dataset_ANDED, field="ID", method="CA",
                          stemming=FALSE, minDegree=10, k.max = 5)

## ---- trend_topics
removed_terms_trend = c("2", "assessment", "use", "analysis", 
                        "performance", "alternative", "green",
                        "process", "environmental", "a.f.", "decarbonization",
                        "carbon footprint", "saf", "sustainability", "aviation",
                        "biomass", "climate change", "energy")
TrendTopics <- fieldByYear(dataset_ANDED, field = "DE", timespan = c(2017,2023), min.freq = 5, n.items = 3, remove.terms = removed_terms_trend, synonyms = synonyms_list, dynamic.plot=FALSE, graph = TRUE)

## ---- aircraft_design_filter
library(dplyr)
df_aircraft_design <- filter(dataset_ANDED, grepl("AIRCRAFT DESIGN", DE))

## ---- aircraft_design_filter_results
results_aircraft_design <- biblioAnalysis(df_aircraft_design, sep = ";")
S_aircraft_design <- summary(object = results_aircraft_design, k = 10, pause = FALSE)
S_aircraft_design

## ---- aircraft_design_keyword_network
NetMatrix_kw_AD <- biblioNetwork(df_aircraft_design, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";");

par(mar=c(0,0,0,0))
net_kw_AD=networkPlot(NetMatrix_kw_AD,
                   n = 30,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=TRUE,
                   labelsize = 1,
                   label.cex = FALSE,
                   label.color = TRUE,
                   cluster = "none",
                   community.repulsion = 0,
                   remove.isolates = FALSE,
                   noloops = TRUE,
                   edgesize = 2,
                   alpha = 0.8,
                   halo = FALSE,
                   curved = FALSE);
## ---- aircraft_design_wordcloud
library(wordcloud2)

Tab_AD <- tableTag(df_aircraft_design, Tag = "TI", sep = ";");
Tab_AD <- log(Tab_AD);
wc_AD <- wordcloud2(Tab_AD, size = 1, minSize = 0, gridSize =  10,
                 fontFamily = 'Liberation Serif', fontWeight = 'bold',
                 color = 'random-dark', backgroundColor = "white",
                 minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
                 rotateRatio = 0, shape = 'circle', ellipticity = 0.8,
                 widgetsize = NULL, figPath = NULL, hoverFunction = NULL);
wc_AD

## ---- term_search_and_exproration
library(dplyr)
df <- filter(dataset_ANDED, grepl("TIANJIN UNIV", affiliations))

results_term <- biblioAnalysis(df, sep = ";")
S_term <- summary(object = results_term, k = 10, pause = FALSE)
S_term

NetMatrix_term <- biblioNetwork(df, analysis = "co-occurrences",
                                 network = "author_keywords", sep = ";");
# par(mar=c(0,0,0,0))
net_term=networkPlot(NetMatrix_term,
                      n = 30,
                      normalize = "association",
                      Title = NULL,
                      type = "fruchterman",
                      size = 10,
                      size.cex = TRUE,
                      remove.multiple=TRUE,
                      labelsize = 0.8,
                      label.cex = FALSE,
                      label.color = TRUE,
                      cluster = "none",
                      community.repulsion = 0,
                      remove.isolates = FALSE,
                      noloops = TRUE,
                      edgesize = 2,
                      alpha = 0.8,
                      halo = FALSE,
                      curved = FALSE);

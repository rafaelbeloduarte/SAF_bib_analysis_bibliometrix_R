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

library(bibliometrix)
library(wordcloud2)

# for a shiny UI for bibliometrix package run
# biblioshiny()

# to have better control of the charts generation we run our on commands
# they are documented on the bibliometrix manual: https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf

# set working directory
setwd("~/DriveUEMEncrypt/casa/uem/Doutorado/Revisão/SAF_bibliometric_analysis/bib_analysis_bibliometrix_R")

load("complete_dataset_ANDED.RData")

results <- biblioAnalysis(dataset_ANDED, sep = ";")
saveRDS(dataset_ANDED, file = "dataset_ANDED_for_biblioshiny.rds")
S <- summary(object = results, k = 10, pause = FALSE)

# author collaboration network
M_AU <- metaTagExtraction(dataset_ANDED, Field = "AU", sep = ";");
NetMatrix_AU <- biblioNetwork(M_AU, analysis = "collaboration", network = "authors", sep = ";");
net_AU=networkPlot(NetMatrix_AU,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "fruchterman",
                   size = 10,
                   size.cex = TRUE,
                   remove.multiple=FALSE,
                   label = TRUE,
                   labelsize= 2,
                   label.color = TRUE,
                   cluster = "edge_betweenness",
                   community.repulsion = 0.0001,
                   label.cex = TRUE,
                   remove.isolates = TRUE,
                   edgesize = 1,
                   alpha = 0.7,
                   halo = FALSE);

# authors keywords co-occurrence network
removed_terms = c("1",
                  "article",
                  )
synonyms_list=c("kinetics;kinetic study",
                );
NetMatrix_kw <- biblioNetwork(dataset_ANDED, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";",
                              synonyms = synonyms_list,
                              remove.terms = removed_terms);
net_kw=networkPlot(NetMatrix_kw,
                   n = 50,
                   normalize = "association",
                   Title = NULL,
                   type = "auto",
                   size = 3,
                   size.cex = FALSE,
                   remove.multiple=FALSE,
                   labelsize= 1,
                   label.cex = FALSE,
                   cluster = "walktrap",
                   community.repulsion = 0.05,
                   remove.isolates = TRUE,
                   edgesize = 10,
                   alpha = 0.50,
                   halo = FALSE);
net2VOSviewer(net_kw);

# word cloud
Tab <- tableTag(dataset_ANDED, Tag = "DE", sep = ";",
                synonyms = synonyms_list, remove.terms = removed_terms);
Tab <- log(Tab);
wordcloud2(Tab, size = 0.3, minSize = 0, gridSize =  10,
           fontFamily = 'Liberation Serif', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
           rotateRatio = 0, shape = 'circle', ellipticity = 0.8,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL);

# country collaboration network
M_CO <- metaTagExtraction(dataset_ANDED, Field = "AU_CO", sep = ";");
NetMatrix_CO <- biblioNetwork(M_CO, analysis = "collaboration", network = "countries", sep = ";");
net_CO=networkPlot(NetMatrix_CO,
                n = dim(NetMatrix_CO)[1],
                normalize = "association",
                Title = NULL,
                type = "fruchterman",
                size = 2,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize=2,
                cluster = "edge_betweenness",
                community.repulsion = 0.01,
                label.cex = TRUE,
                remove.isolates = TRUE,
                edgesize = 5,
                alpha = 0.7);
net2VOSviewer(net_CO);

# collaboration network between institutions
M_AFF <- metaTagExtraction(dataset_ANDED, Field = "AU1_UN", sep = ";");
NetMatrix_AFF <- biblioNetwork(M_AFF, analysis = "collaboration", network = "universities", sep = ";");
net_AFF=networkPlot(NetMatrix_AFF,
                n = 30,
                normalize = "association",
                Title = NULL,
                type = "auto",
                size = 10,
                size.cex = TRUE,
                remove.multiple=FALSE,
                labelsize=2,
                cluster = "edge_betweenness",
                community.repulsion = 0.01,
                label.cex = TRUE,
                remove.isolates = FALSE,
                edgesize = 20,
                alpha = 0.7);

# historical citation network
histResults <- histNetwork(dataset_ANDED, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, size = 10)

# reference publication year spectrocopy
rpys(dataset_ANDED, sep = ";", timespan = c(1950,2022), graph = T)

# historiograph
histResults <- histNetwork(dataset_ANDED, min.citations = 1, sep = ";", network = TRUE, verbose = TRUE);
histPlot(
  histResults,
  n = 20,
  size = 5,
  labelsize = 5,
  title_as_label = FALSE,
  verbose = TRUE
);

# co-citation analysis
NetMatrix_coc <- biblioNetwork(dataset_ANDED, analysis = "co-citation", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_coc, n = 30, Title = "Co-Citation Network", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                remove.multiple=FALSE, labelsize=0.7,edgesize = 5, label.cex = TRUE, community.repulsion = 0.01);

# bib coupling
NetMatrix_bib_coup <- biblioNetwork (dataset_ANDED, analysis = "coupling", network = "references", sep = ";");
net_coc=networkPlot(NetMatrix_bib_coup, n = 50, Title = "Bibliographic coupling", type = "fruchterman", 
                    size=10, size.cex = TRUE,
                    remove.multiple=FALSE, labelsize=2,edgesize = 1, label.cex = TRUE, community.repulsion = 0.01);

# conceptual structure
CS <- conceptualStructure(dataset_ANDED, field="DE", method="CA", 
                          stemming=TRUE, minDegree=10, k.max = 5, synonyms = synonyms_list);

years=c(2005,2010,2015)
nexus <- thematicEvolution(dataset_ANDED,field="DE", years=years, n=100,minFreq=2)
plotThematicEvolution(nexus$Nodes,nexus$Edges)

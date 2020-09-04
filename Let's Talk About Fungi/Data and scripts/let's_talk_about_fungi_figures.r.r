setwd("D:/Documents/Kew/Outreach/Understudied fungi")

## Colour schemes for figures ##

library(scales)

show_col(c(wesanderson::wes_palette("Royal2"), "#7a96d3"))

## Silhouette images for figures (from http://phylopic.org/) ##

library(png)

plant.img <- readPNG("PhyloPic.39335c0c.Anthemidae_Asterales_Asterodae_Asteroideae_Chamaemelum_Chamaemelum-fuscatum_Compositae.png")
animal.img <- readPNG("PhyloPic.dd92c3e8.Gregor-Bucher-Max-Farnworth.Cucujiformia_Polyphaga_Tenebrionoidea_Tribolium_Tribolium-castaneum.png")
fungus.img <- readPNG("PhyloPic.afd875a3.Agaricus_Agaricus-campestris.png")
archaea.img <- readPNG("PhyloPic.e4dde003.Stuart-Humphries.Archaea_Biota.png")
bacteria.img <- readPNG("PhyloPic.6f2a49f4.Matt-Crook.Xanthomonadaceae_Xanthomonas_Xanthomonas-oryzae.png")
protist.img <- readPNG("PhyloPic.9c8a877f.Amorphea_Apusomonadida_Apusomonadidae_Apusomonas_Eukarya_Obazoa_Podiata.png")


## FIGURE 1 - TREE OF LIFE ##

library(ggtree)
library(rphylopic)
library(ape)

#Download newick tree file from Hug et al. 2016 (https://www.nature.com/articles/nmicrobiol201648)
download.file(url="https://static-content.springer.com/esm/art%3A10.1038%2Fnmicrobiol.2016.48/MediaObjects/41564_2016_BFnmicrobiol201648_MOESM207_ESM.txt", destfile="tol.tre")

#Read in tree file
ToL <- read.tree("tol.tre")

#Replace eukaryote tip labels in tree
ToL$tip.label <- sub("Archaeplastida", "Plants", ToL$tip.label)
ToL$tip.label <- sub("Eukaryota_Opisthokonta_Metazoa", "Animals", ToL$tip.label)
ToL$tip.label <- sub("Eukaryota_Opisthokonta_Fungi", "Fungi", ToL$tip.label)
ToL$tip.label <- sub("Eukaryota", "Protists", ToL$tip.label)

#Group tips by first word in tip labels for colouring
groupInfo <- split(ToL$tip.label, sub("\\_.*", "", ToL$tip.label))
ToL <- groupOTU(ToL, groupInfo)

#Plot tree with images
gg.ToL <- ggtree(ToL, layout="equal_angle", aes(color=group), size=0.3) +
  add_phylopic(animal.img, x=0.2, y=3.6, ysize=0.5, col="#F8AFA8") +
  geom_text(aes(x=0.2, y=3.6), label="Animals", fontface="bold", col="#F8AFA8") +
  add_phylopic(fungus.img, x=0.5, y=3.9, ysize=0.5, col="#FDDDA0") +
  geom_text(aes(x=0.5, y=3.9), label="Fungi", fontface="bold", col="#FDDDA0") +
  add_phylopic(plant.img, x=0, y=3.5, ysize=0.5, col="#74A089") +
  geom_text(aes(x=0, y=3.5), label="Plants", fontface="bold", col="#74A089") +
  add_phylopic(bacteria.img, x=4.8, y=2.5, ysize=0.5, col="#9A8822") +
  geom_text(aes(x=4.8, y=2.5), label="Bacteria", fontface="bold", col="#9A8822") +
  add_phylopic(archaea.img, x=0.5, y=2, ysize=0.3, col="#F5CDB4") +
  geom_text(aes(x=0.5, y=2), label="Archaea", fontface="bold", col="#F5CDB4") +
  add_phylopic(protist.img, x=1.5, y=3, ysize=0.2, col="#B4BBD1") +
  geom_text(aes(x=1.5, y=3), label="Protists", fontface="bold", col="#B4BBD1") +
  scale_color_manual(values=c("#F8AFA8", "#F5CDB4", "#9A8822", "#FDDDA0", "#74A089", "#B4BBD1")) +
  geom_text(aes(x=1, y=0.5), label="Tree data from Hug et al. 2016", col="black") +
  theme(legend.position="none")

#As the ggtree tree branches are hard to see, plot the tree with the ape package and use its branching pattern
plot(ToL, type="unrooted", show.tip.label=FALSE)
xx <- get("last_plot.phylo", envir=.PlotPhyloEnv)
gg.ToL$data$y <- xx$yy
gg.ToL$data$x <- xx$xx

#Write to file
tiff(file=paste0("figure_ToL_", Sys.Date(),".tiff"), height=8, width=12, res=300, units="in")
gg.ToL
dev.off()


## FIGURE 2 - DESCRIBED VS ESTIMATED DIVERSITY ##

library(ggplot2)
library(scales)
library(waffle)
library(hrbrthemes)
library(ggplotify)

#Make dataframe of estimates and described eukaryote kingdom diversity according to Larson et al. 2017 (https://www.journals.uchicago.edu/doi/10.1086/693564)
diversity <- data.frame(group=rep(c("animal","fungi","plant"), times=2), 
                        num=c(163, 166, 0.4, 1.5, 0.15, 0.3), 
                        type=rep(c("Total estimated diversity", "Total described diversity"), each=3), 
                        stringsAsFactors=TRUE)

#Change fontface for facet labels
levels(diversity$type) <- c("Total described diversity"=expression(paste("Total ", bold("described "), "diversity")), "Total estimated diversity"=expression(paste("Total ", bold("estimated "), "diversity")))

#Create a dataframe to add legend to one facet
add.legend <- data.frame(x=15, y=27, lab="\U25A0 = 100,000 species", type=factor(expression(paste("Total ", bold("described "), "diversity")), levels=c("paste(\"Total \", bold(\"described \"), \"diversity\")", "paste(\"Total \", bold(\"estimated \"), \"diversity\")")))

#Plot diversity estimates as waffle plot
gg.diversity <- ggplot(diversity, aes(fill=group, values=num*10)) +
  geom_waffle(color="white", size=0.6, n_rows=30, flip=FALSE, alpha=1) +
  facet_wrap(~type, labeller=label_parsed, ncol=1) +
  #add_phylopic(animal.img, x=10, y=10, ysize=13, col="#f8afa8", alpha=1) +
  #add_phylopic(fungus.img, x=2027, y=0.14, ysize=13, col="#fddda0", alpha=1) +
  #add_phylopic(plant.img, x=2027, y=0.04, ysize=13, col="#74a089", alpha=1) +
  geom_text(data=add.legend, aes(x=x, y=y, label=lab), size=3, inherit.aes=FALSE) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0), "mm")) +
  labs(caption="Estimates from Larson et al. 2017")

#Custom function to add images to one facet (https://stackoverflow.com/questions/44688623/adding-custom-images-to-ggplot-facets)
annotation_custom2 <- function (grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, data) { 
  layer(data=data, stat=StatIdentity,
        position=PositionIdentity, 
        geom=ggplot2:::GeomCustomAnn,
        inherit.aes=TRUE, 
        params=list(grob=grob, 
                    xmin=xmin, 
                    xmax=xmax, 
                    ymin=ymin,
                    ymax=ymax))
}

#Make dummy dataframe
dummy <- data.frame(x=1, y=1)
#Make grobs of the images for each eukaryote kingdom
animal.grob <- as.grob(ggplot(dummy, aes(x=x, y=y)) +
                         add_phylopic(animal.img, x=0.5, y=0.5, ysize=1, col="#f8afa8", alpha=1) +
                         theme_void())
plant.grob <- as.grob(ggplot(dummy, aes(x=x, y=y)) +
                        add_phylopic(plant.img, x=0.5, y=0.5, ysize=1, col="#74A089", alpha=1) +
                        theme_void())
fungus.grob <- as.grob(ggplot(dummy, aes(x=x, y=y)) +
                         add_phylopic(fungus.img, x=0.5, y=0.5, ysize=1, col="#FDDDA0", alpha=1) +
                         theme_void())

#Make custom annotation for the top facet with the grobs
animal.annotation <- annotation_custom2(animal.grob, xmin=5, xmax=15, ymin=5, ymax=15, data=diversity[4,])
plant.annotation <- annotation_custom2(plant.grob, xmin=19, xmax=29, ymin=5, ymax=15, data=diversity[6,])
fungus.annotation <- annotation_custom2(fungus.grob, xmin=12, xmax=22, ymin=5, ymax=15, data=diversity[5,])

#Write to file
tiff(file=paste0("figure_diversity_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.diversity + 
  animal.annotation +
  plant.annotation +
  fungus.annotation
dev.off()


## FIGURE 3 - FUNGAL LIFESTYLES ##

library(ggplot2)
library(packcircles)
library(stringr)

#Get function to download FUNGuild database from https://rdrr.io/github/vmikk/metagMisc/man/parse_funguild.html
parse_funguild <- function(url = 'http://www.stbates.org/funguild_db.php', tax_name = TRUE){
  
  # require(XML)
  # require(jsonlite)
  # require(RCurl)
  
  ## Parse data
  tmp <- XML::htmlParse(url)
  tmp <- XML::xpathSApply(doc = tmp, path = "//body", fun = XML::xmlValue)
  
  ## Read url and convert to data.frame
  db <- jsonlite::fromJSON(txt=tmp)
  
  ## Remove IDs
  db$`_id` <- NULL
  
  if(tax_name == TRUE){
    
    ## Code legend
    ## Taxon Level: A numeral corresponding the correct taxonomic level for the taxon
    taxons <- c(
      "keyword",                                                       # 0
      "Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder", # 3:8
      "Family", "Subfamily", "Tribe", "Subtribe", "Genus",             # 9:13
      "Subgenus", "Section", "Subsection", "Series", "Subseries",      # 15:19
      "Species", "Subspecies", "Variety", "Subvariety", "Form",        # 20:24
      "Subform", "Form Species")
    
    ## Table with coding
    taxmatch <- data.frame(
      TaxID = c(0, 3:13, 15:26),
      Taxon = factor(taxons, levels = taxons))
    
    ## Match taxon codes
    db$taxonomicLevel <- taxmatch[match(x = db$taxonomicLevel, table = taxmatch$TaxID), "Taxon"]
  }
  
  # remove rows with missing data
  # which(
  # 	with(db, trophicMode == "NULL" & guild == "NULL" & growthForm == "NULL" & trait == "NULL" & notes == "NULL")
  # 	)
  
  ## Add database dump date as attributes to the result
  attr(db, "DownloadDate") <- date()
  
  return(db)
}

#Download FUNguild database
funguild <- parse_funguild(url="http://www.stbates.org/funguild_db.php", tax_name=TRUE)

#Generalise guild terms
funguild$guild <- sub("Lichenized", "Lichenised", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Lichen parasite", "Lichenicolous", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Fungal Parasite", "Mycoparasite", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Associated Biotroph", "Pathogen", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Root", "Plant", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Plant Associate Biotroph", "Plant Pathogen", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Nematophagous", "Animal Pathogen", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Algal", "Plant", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Bryophyte", "Plant", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Liverwort", "Plant", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Insect", "Animal", funguild$guild, ignore.case=TRUE)
funguild$guild <- sub("Pathotroph", "Pathogen", funguild$guild, ignore.case=TRUE)

#Make dataframe to collect results for number of records for each lifestyle in FUNguild
lifestyles <- data.frame(Guild=c("Lichenised", "Endophyte", "Mycorrhizal", "Plant Pathogen", "Animal Pathogen", "Saprotroph", "Lichenicolous", "Mycoparasite", "Endosymbiont", "Epiphyte", "Animal Parasite"),
                         Colour=c("#74A089", "#74A089", "#74A089", "#74A089", "#F8AFA8", "#806a45", "#FDDDA0", "#FDDDA0", "#F8AFA8", "#74A089", "#F8AFA8"),
                         Num=NA)

#For each lifestyle...
for (i in 1:length(lifestyles$Guild)) {
  #Count number of records and add to results dataframe
  lifestyles$Num[i] <- length(agrep(lifestyles$Guild[i], funguild$guild))
}

#Get x, y and radius for centre of circles to plot
packing <- circleProgressiveLayout(lifestyles$Num, sizetype="area")
#Add a small gap between circles
packing$radius <- packing$radius * 0.9
#Combine lifestyles and plotting dataframes
packing.data <- cbind(lifestyles, packing)
#Create dataframe of lines for each shape
packing.shapes <- circleLayoutVertices(packing, npoints=50)

#Plot as circles
gg.lifestyles <- ggplot() + 
  geom_polygon(data=packing.shapes, aes(x, y, group=id, fill=as.factor(id)), colour="white", alpha=0.3) +
  geom_text(data=packing.data, aes(x, y, size=Num, colour=Colour,label=str_wrap(Guild, width=5)), fontface="bold") +
  geom_text(aes(x=60, y=60, label="Data from FUNguild"), size=1.5) +
  scale_fill_manual(values=lifestyles$Colour) +
  scale_colour_manual(values=c("#74A089", "#806a45", "#F8AFA8", "#FDDDA0")) +
  scale_size_continuous(range=c(1,5)) +
  coord_equal() +
  theme_void() + 
  theme(legend.position="none")
  
#Write to file
tiff(file=paste0("figure_lifestles_", Sys.Date(),".tiff"), height=5, width=4, res=500, units="in")
gg.lifestyles
dev.off()


## FIGURE 4 - EMERGING FUNGAL DISEASES ##

library(ggplot2)
library(rphylopic)
library(treemapify)
library(egg)

#Make dataframe of disease events according to Fisher et al. 2012 (https://www.nature.com/articles/nature10947)
EID <- data.frame(Pathogen=rep(c("Fungi", "Protist", "Viruses", "Others"), times=2),
           Events=c(11, 3, 2, 1, 4, 3, 0, NA),
           Host=rep(c("Animal", "Plant"), each=4))

#Plot as treemaps
gg.EID.animal <- ggplot(EID[EID$Host == "Animal",], aes(area=Events, fill=Pathogen, label=Events)) +
  geom_treemap(colour="white", show.legend=FALSE) +
  geom_treemap_text(colour="white", place="centre", grow=FALSE) +
  scale_fill_manual(values=c("#fddda0", "grey", "#B4BBD1", "darkgrey")) +
  add_phylopic(animal.img, x=-0.2, y=0.5, ysize=0.6, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=0.42, y=0.45, ysize=0.3, col="white", alpha=1) +
  add_phylopic(protist.img, x=0.95, y=0.225, ysize=0.1, col="white", alpha=1) +
  coord_cartesian(xlim=c(0, 1), clip="off") +
  ggtitle("Causes of published disease-driven animal and plant extinction/extirpation events") +
  theme(plot.margin=unit(c(0,25,5,40),"mm"),
        plot.title=element_text(size=12, margin=margin(0,0,20,0), hjust=0.5, face="bold"))

gg.EID.plant <- ggplot(EID[EID$Host == "Plant",], aes(area=Events, fill=Pathogen, label=Events)) +
  geom_treemap(colour="white", show.legend=FALSE) +
  geom_treemap_text(colour="white", place="centre", grow=FALSE) +
  scale_fill_manual(values=c("#fddda0", "grey", "#B4BBD1", "darkgrey")) +
  add_phylopic(plant.img, x=-0.2, y=0.5, ysize=0.6, col="#74a089", alpha=1) +
  add_phylopic(fungus.img, x=0.35, y=0.45, ysize=0.3, col="white", alpha=1) +
  add_phylopic(protist.img, x=0.9, y=0.5, ysize=0.1, col="white", alpha=1) +
  coord_cartesian(xlim=c(0, 1), clip="off") +
  theme(plot.margin=unit(c(5,25,0,40),"mm")) +
  labs(caption="Data from Fisher et al. 2012")

#Write to file
tiff(file=paste0("figure_EID_", Sys.Date(),".tiff"), height=5, width=8, res=300, units="in")
egg::ggarrange(gg.EID.animal, gg.EID.plant, nrow=2)
dev.off()


## FIGURE 5 - LIFESTYLE OVERLAP ##

library(dplyr)
library(eulerr)

#Get function to download FUNGuild database from https://rdrr.io/github/vmikk/metagMisc/man/parse_funguild.html
parse_funguild <- function(url = 'http://www.stbates.org/funguild_db.php', tax_name = TRUE){
  
  # require(XML)
  # require(jsonlite)
  # require(RCurl)
  
  ## Parse data
  tmp <- XML::htmlParse(url)
  tmp <- XML::xpathSApply(doc = tmp, path = "//body", fun = XML::xmlValue)
  
  ## Read url and convert to data.frame
  db <- jsonlite::fromJSON(txt=tmp)
  
  ## Remove IDs
  db$`_id` <- NULL
  
  if(tax_name == TRUE){
    
    ## Code legend
    ## Taxon Level: A numeral corresponding the correct taxonomic level for the taxon
    taxons <- c(
      "keyword",                                                       # 0
      "Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder", # 3:8
      "Family", "Subfamily", "Tribe", "Subtribe", "Genus",             # 9:13
      "Subgenus", "Section", "Subsection", "Series", "Subseries",      # 15:19
      "Species", "Subspecies", "Variety", "Subvariety", "Form",        # 20:24
      "Subform", "Form Species")
    
    ## Table with coding
    taxmatch <- data.frame(
      TaxID = c(0, 3:13, 15:26),
      Taxon = factor(taxons, levels = taxons))
    
    ## Match taxon codes
    db$taxonomicLevel <- taxmatch[match(x = db$taxonomicLevel, table = taxmatch$TaxID), "Taxon"]
  }
  
  # remove rows with missing data
  # which(
  # 	with(db, trophicMode == "NULL" & guild == "NULL" & growthForm == "NULL" & trait == "NULL" & notes == "NULL")
  # 	)
  
  ## Add database dump date as attributes to the result
  attr(db, "DownloadDate") <- date()
  
  return(db)
}

#Download FUNguild database
funguild <- parse_funguild(url="http://www.stbates.org/funguild_db.php", tax_name=TRUE)

#Fix trophic terms to be uniform
funguild$trophicMode <- sub("Pathogen", "Pathotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Sybiotroph", "Symbiotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Saprotroph-Pathotroph", "Pathotroph-Saprotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Saprotroph-Pathotroph-Symbiotroph", "Pathotroph-Saprotroph-Symbiotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Saportroph", "Saprotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Pathotroph-Saptroroph", "Pathotroph-Saprotroph", funguild$trophicMode)
funguild$trophicMode <- sub("Symbiotroph-Saprotroph", "Saprotroph-Symbiotroph", funguild$trophicMode)

#Summarise numbers in sets
funguild.sum <- funguild %>%
  group_by(trophicMode) %>%
  tally()

#Replace hyphens with & for eulerr package
funguild.sum$trophicMode <- gsub("-", "&", funguild.sum$trophicMode)
#Convert into named vector
funguild.sum <- setNames(as.vector(funguild.sum$n), funguild.sum$trophicMode)

#Create euler diagram
eul.funguild <- euler(funguild.sum)

#Plot to file
tiff(file=paste0("figure_trophic_", Sys.Date(),".tiff"), height=3, width=4, res=300, units="in")
plot(eul.funguild,
     shape="ellipse",
     fills=list(fill=c("#F5CDb4", "#806a45", "#7A96D3"), alpha=0.3),
     labels=list(col=c("#F5CDb4", "#806a45", "#7A96D3")),
     edges=FALSE,
     main=list(label="Data from FUNguild", cex=0.6))
dev.off()


## FIGURE 6 - PUBLISHED PAPERS ##

library(europepmc)
library(ggplot2)
library(rphylopic)
library(scales)

#For each eukaryote kindgom...
for (i in c("animal", "fungi", "plant")) {
  
  #Get paper publishing stats
  trend <- epmc_hits_trend(query=i, 
                           period=1950:2020)
  trend$type <- i
  assign(paste0(i, ".trend"), trend)
  
}

#Combine trend dataframes
all.trend <- rbind(fungi.trend, animal.trend, plant.trend)

#Plot publishing trends as bargraph
gg.trend <- ggplot(all.trend, aes(x=year, y=(query_hits / all_hits), fill=type)) +
  geom_col(width=0.6, alpha=1) +
  add_phylopic(animal.img, x=2027, y=0.3, ysize=13, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=2027, y=0.14, ysize=13, col="#fddda0", alpha=1) +
  add_phylopic(plant.img, x=2027, y=0.04, ysize=13, col="#74a089", alpha=1) +
  scale_y_continuous(expand=c(0, 0), limits=c(0,0.4)) +
  scale_x_continuous(expand=c(0.01, 0.01), breaks=pretty_breaks()) +
  coord_cartesian(xlim=c(1950, 2020), clip="off") +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  labs(x="Year", y="Proportion of all published articles") +
  ggtitle("Representation of the eukaryote kingdoms in Europe PMC", subtitle=format(Sys.Date(), format="%d %B %Y")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(0,20,0,5),"mm"),
        legend.position="none",
        plot.title=element_text(size=10, margin=margin(0,0,5,0), face="bold"),
        plot.subtitle=element_text(size=8, margin=margin(0,0,5,0), hjust=0.02),
        axis.title.x=element_text(size=8, margin=margin(7,0,0,0)),
        axis.title.y=element_text(size=8, margin=margin(0,7,0,0)))

#Write to file
tiff(file=paste0("figure_papers_", Sys.Date(),".tiff"), height=6, width=7, res=300, units="in")
gg.trend
dev.off()


## FIGURE 7 - IUCN REDLIST ##

library(ggplot2)
library(rphylopic)
library(ggstance)

#Make dataframe of redlisted species as of 2/9/2020 (https://www.iucnredlist.org/search?searchType=species)
redlist <- data.frame(Taxon=c("Animals", "Plants", "Fungi"),
                      Number=c(76457, 43556, 343))
redlist$Taxon <- factor(redlist$Taxon, levels=c("Animals", "Plants", "Fungi"))

#Plot number of redlisted species in eukaryote kingdoms as bargraph
gg.redlist <- ggplot(redlist, aes(y=Taxon, fill=Taxon, x=Number)) +
  geom_barh(stat="identity", width=0.5) +
  add_phylopic(animal.img, x=-10, y=1, ysize=20, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=-10, y=2, ysize=20, col="#fddda0", alpha=1) +
  add_phylopic(plant.img, x=-10, y=3, ysize=20, col="#74a089", alpha=1) +
  scale_fill_manual(values=c("#f8afa8", "#74a089", "#fddda0")) +
  scale_x_continuous(expand=c(0,0)) +
  theme_void() +
  theme(legend.position="none")

#Write plot to file
tiff(file=paste0("figure_redlist_", Sys.Date(),".tiff"), height=3, width=10, res=300, units="in")
gg.redlist
dev.off()


## FIGURE 8 - HIGHER EDUCATION ##

library(ggplot2)
library(rphylopic)
library(ggstance)
library(stringr)
library(dplyr)
library(ggrepel)
library(egg)

#Read in results from python webscrape of undergrad courses
edu.courses <- read.csv("whatuni_courses.csv")
#Order dataframe alphabetically
edu.courses <- edu.courses[order(edu.courses$eukaryote),]

#Plot number of courses for eukaryote kingdoms as bargraph
gg.edu.courses <- ggplot(edu.courses, aes(y=eukaryote, fill=eukaryote, x=count)) +
  geom_barh(stat="identity") +
  geom_text(aes(label=count, x=count, y=eukaryote), size=5, position=position_stack(vjust=0.5), fontface="bold", colour="white") +
  add_phylopic(animal.img, x=-13.8, y=1, ysize=27.8, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=-13.8, y=2, ysize=27.8, col="#fddda0", alpha=1) +
  add_phylopic(plant.img, x=-13.8, y=3, ysize=27.8, col="#74a089", alpha=1) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(0, 180), clip="off") +
  ggtitle("Number of UK undergraduate courses for eukaryote kingdoms") +
  theme_void() +
  theme(legend.position="none",
        plot.title=element_text(margin=margin(0,0,5,0), face="bold"),
        plot.margin=unit(c(0,10,0,40),"mm"))

#Write to file
tiff(file=paste0("figure_courses_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.edu.courses
dev.off()

#Read in results from python webscrape of biology undergrad modules
edu.bio <- read.csv("uk_biology_modules.csv")
#Correct capitalisation of university names
edu.bio$uni <- str_to_title(edu.bio$uni)
#Make dataframe summarising number of mentions for each kingdom
edu.bio.sum <- edu.bio %>%
  group_by(eukaryote) %>%
  summarise(count=sum(count))

#Plot number of mentions of eukaryote kingdoms in modules as bargraph
gg.edu.bio <- ggplot(edu.bio.sum, aes(y=eukaryote, fill=eukaryote, x=count)) +
  geom_barh(stat="identity") +
  geom_text(aes(label=count, x=count, y=eukaryote), size=5, position=position_stack(vjust=0.5), fontface="bold", colour="white") +
  add_phylopic(animal.img, x=-10, y=1, ysize=20, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=-10, y=2, ysize=20, col="#fddda0", alpha=1) +
  add_phylopic(plant.img, x=-10, y=3, ysize=20, col="#74a089", alpha=1) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(0, 130), clip="off") +
  ggtitle("Number of mentions in undergraduate biology module descriptions") +
  theme_void() +
  theme(legend.position="none",
        plot.title=element_text(margin=margin(0,0,5,0), face="bold"),
        plot.margin=unit(c(0,10,0,40),"mm"))

#Write to file
tiff(file=paste0("figure_modules_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.edu.bio
dev.off()

#Write to file with uni labels
tiff(file=paste0("figure_modules+label_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.edu.bio +
  geom_text_repel(data=edu.bio[edu.bio$eukaryote == "fungi" & edu.bio$count > 0,], aes(x=8, label=uni), nudge_x=10, size=5, direction="y")
dev.off()

#Plot number of kingdom mentions stacked together
gg.edu.bio.stack <- ggplot(edu.bio.sum, aes(y=2, fill=eukaryote, x=count)) +
  geom_barh(stat="identity") +
  geom_text(aes(label=count, x=count, y=2), size=5, position=position_stack(vjust=0.5), fontface="bold", colour="white") +
  add_phylopic(animal.img, x=-10, y=1.8, ysize=20, col="#f8afa8", alpha=1) +
  add_phylopic(fungus.img, x=-10, y=2, ysize=20, col="#fddda0", alpha=1) +
  add_phylopic(plant.img, x=-10, y=2.2, ysize=20, col="#74a089", alpha=1) +
  scale_y_discrete(limits=c(as.character(1),as.character(2), as.character(3))) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(0, 220), clip="off") +
  ggtitle("Number of mentions in undergraduate biology module descriptions") +
  theme_void() +
  theme(legend.position="none",
        plot.title=element_text(margin=margin(0,0,5,0), face="bold"),
        plot.margin=unit(c(0,10,0,40),"mm"))

#Write to file
tiff(file=paste0("figure_modules+stack_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.edu.bio.stack
dev.off()


## FIGURE 9 - KEW SCIENCE FESTIVAL ##

library(ggplot2)
library(rphylopic)
library(stringr)

#Make dataframe of Kew Science Festival 2019 survey results
scifest <- data.frame(Activity=c("Bees", "Fungi Quest", "More than a mushroom", "Gastronaut/Gastro adventure", "DNA Factory", "Grass Show", "How tall is that tree?", "Cryo corner/icecream"),
                      Mentions=c(38, 22, 18, 14, 14, 12, 11, 10),
                      Fungi=c("N", "Y", "Y", "N", "N", "N", "N", "N"))
#Order activities by number of mentions
scifest$Activity <- factor(scifest$Activity, levels=scifest$Activity[order(-scifest$Mentions)])

#Plot number of activity mentions as bargraph
gg.scifest <- ggplot(scifest, aes(x=Activity, y=Mentions, fill=Fungi)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Mentions, x=Activity, y=Mentions), size=5, position=position_stack(vjust=0.5), fontface="bold", colour="white") +
  add_phylopic(fungus.img, x=2, y=15, ysize=5, col="white", alpha=1) +
  add_phylopic(fungus.img, x=3, y=13, ysize=5, col="white", alpha=1) +
  scale_fill_manual(values=c("grey", "#fddda0")) +
  scale_x_discrete(labels=function(x) str_wrap(x, width=10)) +
  ggtitle("Visitors' most enjoyed activities at Kew Science Festival 2019 (out of 39 total activities)") +
  theme_void() +
  theme(legend.position="none",
        axis.text.x=element_text(size=8),
        plot.title=element_text(size=10, margin=margin(0,0,5,0), face="bold"))

#Write plot to file
tiff(file=paste0("figure_scifest_", Sys.Date(),".tiff"), height=5, width=8, res=300, units="in")
gg.scifest
dev.off()

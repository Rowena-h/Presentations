setwd("D:/Documents/Kew/Outreach/Understudied fungi")

##Colour schemes for figures##

library(scales)

show_col(c(wesanderson::wes_palette("Royal2"), "#7a96d3"))

##Silhouette images for figures (from http://phylopic.org/)##

library(png)

plant.img <- readPNG("PhyloPic.39335c0c.Anthemidae_Asterales_Asterodae_Asteroideae_Chamaemelum_Chamaemelum-fuscatum_Compositae.png")
animal.img <- readPNG("PhyloPic.dd92c3e8.Gregor-Bucher-Max-Farnworth.Cucujiformia_Polyphaga_Tenebrionoidea_Tribolium_Tribolium-castaneum.png")
fungus.img <- readPNG("PhyloPic.afd875a3.Agaricus_Agaricus-campestris.png")
archaea.img <- readPNG("PhyloPic.e4dde003.Stuart-Humphries.Archaea_Biota.png")
bacteria.img <- readPNG("PhyloPic.6f2a49f4.Matt-Crook.Xanthomonadaceae_Xanthomonas_Xanthomonas-oryzae.png")
protist.img <- readPNG("PhyloPic.9c8a877f.Amorphea_Apusomonadida_Apusomonadidae_Apusomonas_Eukarya_Obazoa_Podiata.png")


##FIGURE 1 - TREE OF LIFE ##

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


##FIGURE 2 - STUDIED VS TOTAL DIVERSITY ESTIMATE##

library(europepmc)
library(ggplot2)
library(rphylopic)
library(waffle)
library(hrbrthemes)
library(ggpubr)

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
        plot.title=element_text(size=10, margin=margin(0,0,5,0), hjust=0.5, face="bold"),
        plot.subtitle=element_text(size=8, margin=margin(0,0,5,0), hjust=0.02),
        axis.title.x=element_text(size=8, margin=margin(7,0,0,0)),
        axis.title.y=element_text(size=8, margin=margin(0,7,0,0)))
  

#Make dataframe of estimates and described eukaryote kingdom diversity according to Larson et al. 2017 (https://www.journals.uchicago.edu/doi/10.1086/693564)
diversity <- data.frame(group=rep(c("animal","fungi","plant"), times=2), 
                        num=c(163, 166, 0.4, 1.5, 0.15, 0.3), 
                        type=rep(c("Total estimated diversity", "Total described diversity"), each=3), 
                        stringsAsFactors=TRUE)

levels(diversity$type) <- c("Total described diversity"=expression(paste("Total ", bold("described "), "diversity")), "Total estimated diversity"=expression(paste("Total ", bold("estimated "), "diversity")))


add.legend <- data.frame(x=15, y=27, lab="\U25A0 = 100,000 species", type=factor(expression(paste("Total ", bold("described "), "diversity")), levels=c("paste(\"Total \", bold(\"described \"), \"diversity\")", "paste(\"Total \", bold(\"estimated \"), \"diversity\")")))

gg.diversity <- ggplot(diversity, aes(fill=group, values=num*10)) +
  geom_waffle(color="white", size=0.6, n_rows=30, flip=FALSE, alpha=1) +
  facet_wrap(~type, labeller=label_parsed, ncol=1) +
  geom_text(data=add.legend, aes(x=x, y=y, label=lab), size=3, inherit.aes=FALSE) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0), "mm")) +
  labs(caption="Estimates from Larson et al. 2017")

#Plot to file
tiff(file=paste0("figure_papers+diversity_", Sys.Date(),".tiff"), height=5, width=10, res=300, units="in")
ggpubr::ggarrange(gg.trend, gg.diversity, ncol=2, widths=c(1.2,1))
dev.off()


##FIGURE 3 - BIOMASS##

library(ggplot2)
library(rphylopic)
library(treemapify)

#Make dataframe of global biomass of the eukaryote kingdoms according to Bar-On, Phillips & Milo 2017 (https://www.pnas.org/content/115/25/6506)
biomass <- data.frame(Taxon=c("Plants", "Bacteria", "Fungi", "Archaea", "Protists", "Animals"),
           Mass=c(450, 70, 12, 7, 4, 2),
           Uncertainty=c(1.2, 10, 3, 13, 4, 5))


gg.biomass <- ggplot(biomass[c(1,3,6),], aes(area=Mass, fill=Taxon, label=Mass)) +
  geom_treemap(colour="white", show.legend=FALSE) +
  geom_treemap_text(colour="white", place="centre", grow=FALSE) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  add_phylopic(animal.img, x=1.1, y=1, ysize=0.1, col="#f8afa8", alpha=1) +
  add_phylopic(plant.img, x=-0.25, y=0.5, ysize=0.6, col="#74a089", alpha=1) +
  add_phylopic(fungus.img, x=1.15, y=0.4, ysize=0.3, col="#fddda0", alpha=1) +
  coord_cartesian(xlim=c(0, 1), clip="off") +
  ggtitle("Gigatonnes of biomass contributed by each eukaryote kingdom") +
  theme(plot.margin=unit(c(0,25,0,40),"mm"),
        plot.title=element_text(size=12, margin=margin(0,0,20,0), hjust=0.5, face="bold"),) +
  labs(caption="Estimates from Bar-On, Phillips & Milo 2017")

tiff(file=paste0("figure_biomass_", Sys.Date(),".tiff"), height=5, width=7, res=300, units="in")
gg.biomass
dev.off()


##FIGURE 4 - HIGHER EDUCATION##

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

#Plot number of kingdom mentions stacked and flipped
gg.edu.bio.stack.flip <- ggplot(edu.bio.sum, aes(x=1, fill=eukaryote, y=count)) +
  geom_col() +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(legend.position="none")

#Write to file
tiff(file=paste0("figure_modules+stack+flip_", Sys.Date(),".tiff"), height=12, width=2, res=300, units="in")
gg.edu.bio.stack.flip
dev.off()

#Plot trend figure without phylopic images
gg.trend2 <- ggplot(all.trend, aes(x=year, y=(query_hits / all_hits), fill=type)) +
  geom_col(width=0.6, alpha=1) +
  scale_y_continuous(expand=c(0, 0), limits=c(0,0.4)) +
  scale_x_continuous(expand=c(0.01, 0.01), breaks=pretty_breaks()) +
  scale_fill_manual(values=c("#f8afa8", "#fddda0", "#74a089")) +
  labs(x="Year", y="Proportion of all published articles") +
  ggtitle("Representation of the eukaryote kingdoms in Europe PMC", subtitle=format(Sys.Date(), format="%d %B %Y")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(0,5,0,5),"mm"),
        legend.position="none",
        plot.title=element_text(size=10, margin=margin(0,0,5,0), hjust=0.5, face="bold"),
        plot.subtitle=element_text(size=8, margin=margin(0,0,5,0), hjust=0.02),
        axis.title.x=element_text(size=8, margin=margin(7,0,0,0)),
        axis.title.y=element_text(size=8, margin=margin(0,7,0,0)))

#Write combined trend and modules plot to file
tiff(file=paste0("figure_papers+stack", Sys.Date(),".tiff"), height=5, width=5, res=300, units="in")
egg::ggarrange(gg.trend2, gg.edu.bio.stack.flip, widths=c(20,1))
dev.off()


##IUCN redlist

library(rredlist)

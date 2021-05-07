##########################
### KABaM 2021 FIGURES ###
##########################

## FTOL GRAPHIC (images from https://stateoftheworldsfungi.org/) ##

library(ape)
library(ggtree)
library(ggimage)
library(png)

FToL <- read.tree(text="(((((Basidiomycota,Ascomycota),(Mucoromycota,Zoopagomycota)),Chytridiomyceta),Blastocladiomycota), Opisthosporidia);")

gg.FToL <- ggtree(FToL, layout="circular") +
  xlim(0, 7) +
  geom_label(x=3, y=-0.5, label=expression(paste("PA", bold("F"), "TOL")), fill=NA, label.size=NA, size=8) +
  geom_tiplab(geom="image", aes(image=paste0(label, "_icon.png")), size=0.08)

#Write to file
tiff(file=paste0("FToL_", Sys.Date(),".tiff"), height=5, width=7, res=300, units="in")
gg.FToL
dev.off()


## FIGURE OF DESCRIBED VS ESTIMATED FUNGAL DIVERSITY ##

library(ggplot2)
library(scales)
library(waffle)
library(hrbrthemes)
library(ggplotify)

#Make dataframe of estimates and described fungal diversity according to Baldrian et al. 2021 (https://doi.org/10.1007/s13225-021-00472-y)
diversity <- data.frame(group=rep("fungi", times=2), 
                        num=c(6.28, 0.15), 
                        type=c("Total estimated diversity", "Total described diversity"), 
                        stringsAsFactors=TRUE)

#Change fontface for facet labels
levels(diversity$type) <- c("Total described diversity"=expression(paste("Total ", bold("described "), "diversity")),
                            "Total estimated diversity"=expression(paste("Total ", bold("estimated "), "diversity")))

#Create a dataframe to add legend to one facet
add.legend <- data.frame(x=5, y=-1, lab="\U25A0 = 10,000 species",
                         type=factor(expression(paste("Total ", bold("described "), "diversity")),
                                     levels=c("paste(\"Total \", bold(\"described \"), \"diversity\")",
                                              "paste(\"Total \", bold(\"estimated \"), \"diversity\")")))


gg.diversity <- ggplot(diversity[diversity$group == "fungi",], aes(fill=group, values=num*100)) +
  geom_waffle(color="white", alpha=1, size=0.8) +
  facet_wrap(~type, labeller=label_parsed, ncol=1) +
  geom_text(data=add.legend, aes(x=x, y=y, label=lab), size=4, inherit.aes=FALSE) +
  scale_fill_manual(values=c("#fddda0")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0), "mm")) +
  labs(caption="Estimate from Baldrian et al., 2021")

tiff(file=paste0("diversity_gap_", Sys.Date(),".tiff"), height=6, width=12, res=300, units="in")
gg.diversity
dev.off()


## FIGURE OF ENDOPHYTE LIFESTYLE OVERLAP ##

library(dplyr)
library(eulerr)
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
#Filter for endophytes
funguild <- funguild[grep("Endophyt*", funguild$guild),]
#Filter for species level
funguild <- funguild[funguild$taxonomicLevel == "Species",]
#Make list of guilds for each entry
funguild.guilds <- str_split(as.list(funguild$guild), "-")
#Make list of vectors for each entry
guilds <- str_split(funguild$guild, "-")
#Summarise lifestyles
for (i in c("Saprotroph", "Mycorrhizal")) {
  guilds <- lapply(guilds, function(x) gsub(paste0(".*", i), i, x, ignore.case=TRUE))
}
#Sort vectors alphabetically and remove duplicates
guilds <- lapply(guilds, function(x) sort(unique(x)))

#Make vector for euler
guilds <- unlist(lapply(guilds, function(x) paste(x, collapse="&")))
#Summarise numbers in sets
funguild.sum <- setNames(as.vector(table(guilds)), names(table(guilds)))
#Create euler diagram
eul.funguild <- euler(funguild.sum[6])

set.seed(1)

tiff(file=paste0("endophyte-lifestyle-overlap_", Sys.Date(),".tiff"), height=6, width=6, res=300, units="in")
plot(euler(funguild.sum[6]),
     quantities=list(col="#49ba4a"),
     labels=list(col="#49ba4a"),
     shape="ellipse",
     fills=list(fill="#49ba4a", alpha=0.5),
     edges=list(lty=3, col="#49ba4a"),
     main=list(label="Data from FUNguild", cex=0.7))
dev.off()

set.seed(1)

tiff(file=paste0("endophyte-lifestyle-overlap2_", Sys.Date(),".tiff"), height=6, width=6, res=300, units="in")
plot(euler(funguild.sum),
     shape="ellipse",
     fills=list(fill=c("#70202E", "#49ba4a", "#02401B", "#F4C030", "#884E29", "black", "dimgrey", "#168C71"), alpha=0.5),
     labels=list(col=c("#70202E", "#49ba4a", "#02401B", "#F4C030", "#884E29", "black", "dimgrey", "#168C71")),
     edges=list(lty=3, col=c("#70202E", "#49ba4a", "#02401B", "#F4C030", "#884E29", "black", "dimgrey", "#168C71")),
     main=list(label="Data from FUNguild", cex=0.7))
dev.off()

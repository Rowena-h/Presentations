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

library(reshape2)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

load("outcomes Schizophrenia.RData")

gg.m <- melt(NBspieSchizo$NetBenefit)
colnames(gg.m) <- c("Lambda", "Treatment", "SAWIS_NB")

#display.brewer.all() # can also check colorbrewer2.org

#change line type to more distinguishable ones
format <- data.frame(cbind(colnames(NBspieSchizo$NetBenefit), shape = rep(1:3, 5), color = rep(RColorBrewer::brewer.pal(5, "Set1"),3)))

format$shape <- as.numeric(format$shape)
colnames(format)[1] <- "Treatment"

gg.m <- gg.m %>% left_join(format, by = "Treatment")


ggplot(gg.m, aes(x = Lambda, y = SAWIS_NB, Treatment=Treatment, color = Treatment, linetype = Treatment)) + 
  geom_line(lwd=1.5) +
  scale_color_manual(values = format$color) +
  scale_linetype_manual(values = format$shape) +
  ylab(expression(italic(SAWIS[NB]))) + 
  xlab(expression(bold(lambda))) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 12),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=13, face = "bold"), #change legend title font size
        legend.text = element_text(size=11),
        legend.position = "bottom") + #change legend text font size 
  guides(color=guide_legend(nrow = 3, byrow = T)) 



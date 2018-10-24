#load raw txt data file and rename "gslwp" for simplification

View(StomReg_raw_v01)
gslwp <- StomReg_raw_v01

#transform Gs and LWP values 
#convert column "Mean Gs" from mmol m-2 s-1 -> mol m-2 s-1
gslwp$MeanGs_mol <- gslwp$MeanGs / 1000

View(gslwp)

#convert MeanLWP to negative measurements
gslwp$MeanLWP_neg <- gslwp$MeanLWP * -1

View(gslwp)

#load ggplot2

#preliminary scatterplot for raw mean Gs as a function of raw mean LWP
plot1 <- ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) +geom_point()
plot1

#use different shapes to identify Spring and Summer readings
plot2 <- ggplot(gslwp, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species, shape = as.factor(Season))) +
  geom_point()
plot2

#extract "Spring17" data into new dataframe
gslwp_spring <- gslwp %>% filter(Season == "Spring17")
View(gslwp_spring)
str(gslwp_spring)

#preliminary scatterplot for raw mean Gs as a function of raw mean LWP. SPRING ONLY
plot2 <- ggplot(gslwp_spring, aes(x=MeanLWP_neg, y=MeanGs_mol, color = Species)) +geom_point()
plot2

#produce scatterplot for Rutaceae only with smooth local regression ("loess" used as useful for small number of observations)
gslwp_subset_Ruta <- subset(gslwp_spring, Family %in% c("Rutaceae")==TRUE)

plot_Ruta_Sp <- ggplot(gslwp_subset_Ruta, aes(MeanLWP_neg, MeanGs_mol, color=Species)) + geom_point() +
                         geom_smooth(se=F)
plot_Ruta_Sp

#convert LWP to MPa
gslwp_subset_Ruta$MeanLWP_negMPa <- gslwp_subset_Ruta$MeanLWP_neg /10
plot_Ruta_Sp2 <- ggplot(gslwp_subset_Ruta, aes(MeanLWP_negMPa, MeanGs_mol, color=Species)) +
  geom_point() +
  geom_smooth()

#reverse order of x axis and reset x axis scale
plot_Ruta_Sp3 <- plot_Ruta_Sp2 +scale_x_continuous(trans = "reverse", breaks = unique(gslwp_subset_Ruta$MeanLWP_negMPa))
plot_Ruta_Sp3

#packge "scales" to access break formatting functions
library(scales)

plot_Ruta_Sp4 <- plot_Ruta_Sp3 + xlim(0, -75)
plot_Ruta_Sp5 <- plot_Ruta_Sp4 + scale_x_continuous(trans = "reverse", breaks = waiver())

plot_Ruta_Sp6 <- plot_Ruta_Sp5 + xlim(0, -7)
plot_Ruta_Sp6 <- plot_Ruta_Sp6 + ylim(0, 1)
plot_Ruta_Sp6

#change axis labels
plot_Ruta_Sp6 <- plot_Ruta_Sp6 +xlab("Mean leaf water potential (MPa)")
plot_Ruta_Sp6 <- plot_Ruta_Sp6 +ylab("Mean stomatal conductance (mol m-2 s-1)")
plot_Ruta_Sp6

#add title
plot_Ruta_Sp6 <- plot_Ruta_Sp6 + ggtitle("Stomatal conductance as a function of leaf water potential during the spring 2017 dry season \nSpecies in the Rutaceae family")
plot_Ruta_Sp6

#produce scatterplot for Boraginaceae only with smooth local regression ("loess" used as useful for small number of observations)
#first, add column for LWP in MPa in main Spring dataframe 
gslwp_spring$MeanLWP_negMPa <- gslwp_spring$MeanLWP_neg /10
gslwp_spring$MeanLWP_negMPa <- NULL
gslwp
View(gslwp_spring)

#subset Boraginaceae family
gslwp_subset_Borag <- subset(gslwp_spring, Family %in% c("Boraginaceae")==TRUE)
View(gslwp_subset_Borag)

#create scatterplot
plot_Borag_Sp <- ggplot(gslwp_subset_Borag, aes(MeanLWP_negMPa, MeanGs_mol, color = Species)) + geom_point() +
  geom_smooth(se=F)
plot_Borag_Sp

#reverse x axis and reset y and x axis values
plot_Borag_Sp2 <- plot_Borag_Sp + scale_x_continuous(trans = "reverse", breaks = waiver())
plot_Borag_Sp2 <- plot_Borag_Sp2 + xlim(0, -7)
plot_Borag_Sp2 <- plot_Borag_Sp2 + ylim(0, 1)

#change axis labels and add title
plot_Borag_Sp2 <- plot_Borag_Sp2 +xlab("Mean leaf water potential (MPa)")
plot_Borag_Sp2 <- plot_Borag_Sp2 +ylab("Mean stomatal conductance (mol m-2 s-1)")
plot_Borag_Sp3 <- plot_Borag_Sp2 + ggtitle("Stomatal conductance as a function of leaf water potential during the spring 2017 dry season \nSpecies in the Boraginaceae family")
plot_Borag_Sp3
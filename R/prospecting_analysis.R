library(ggplot2)
library(tidyverse)
library(ggmosaic)
library(wesanderson)
names(wes_palettes)

#read in data, excerpt NA rows at bottom of morph_data
deut_data <- read.csv("Deut.csv", stringsAsFactors = F)
morph_data <- read.csv("Morph.csv", stringsAsFactors = F)
morph_data <- morph_data[1:216,]

#check structure
str(morph_data)

#add status column
deut_data$status <- ifelse(deut_data$Date == "#N/A", "local", "fall") 

#change date format
deut_data$Date[deut_data$Date == "#N/A"] <- NA
deut_data$Julian[deut_data$Julian == "#N/A"] <- NA
deut_data$Julian <- as.numeric(deut_data$Julian)
deut_data$Date <- as.Date(deut_data$Date, format = "%d/%m/%Y")
morph_data$Date[morph_data$Date == "#N/A"] <- NA
morph_data$Julian[morph_data$Julian == "#N/A"] <- NA
morph_data$Date <- as.Date(morph_data$Date, format = "%d/%m/%Y")
morph_data$PAge[morph_data$PAge == "2"] <- "A"
morph_data$PAge[morph_data$PAge == "3"] <- "A"


#extract all the double measured individuals
doubles <- deut_data[duplicated(deut_data$Band) | duplicated(deut_data$Band, fromLast = T),] #keep only duplicated bands
doubles <- doubles[order(doubles$Band, doubles$FeatherType),] #sort by band number and then feather type
doubles <- filter(doubles, doubles$Band != "1833-03340") #remove case with 2 primaries and no juv

#take the difference between the juv and primary feathers for each individual
difs <- c()
for (i in 1:length(unique(doubles$Band))) {
  difs[i] <- doubles$dD[doubles$Band == unique(doubles$Band)[i] 
                        & doubles$FeatherType == "j"] - doubles$dD[doubles$Band == unique(doubles$Band)[i] & 
                                                                     doubles$FeatherType == "p"]
}

#one sampe t test on the pairwise differences to see if there is bias for feather type.
t.test(na.omit(difs), mu = 0)

#functions to get the quantiles of the local distributions lower than the value of each fall unknown bird 
#i.e. reall large (close to 1) or small (close to 0) number indicate loq overlap with the local distribution
get_quant <- function(samp, null) {
  pval <- length(na.omit(null)[na.omit(null) < samp])/length(na.omit(null))
return(pval)
  }
sapply(X = deut_data$dD[deut_data$status == "fall"], FUN = get_quant, null = deut_data$dD[deut_data$status == "local"])

pal <- wes_palette("Zissou1", 3, type = "discrete")

#plot histograms of dD values for known local birds and fall captures
deut_hist <- ggplot(data = deut_data) +
  geom_density(aes(x = dD, fill = status), alpha = .25) + 
  xlab(expression(paste(delta, "D", sep = "")))

# #historgram of difs
# (difs_hist <- ggplot(data = data.frame(dD = difs)) +
#   geom_density(aes(x = dD), alpha = .25, color = as.character(pal[1]), 
#                fill = as.character(pal[2])) +
#   xlab(expression(paste("Difference in ", delta, "D", sep = ""))) +
#   geom_vline(xintercept = mean(difs)) +
#   geom_rug(aes(x = dD)) +
#   theme_minimal())
# 
# (pairs_plot <- ggplot(data = doubles, 
#                       aes(x = FeatherType, y = dD, group = Band)) +
#   geom_point() +
#   geom_line())
#   
#save figure
ggsave("figures/delta_deut_hist.pdf", plot = deut_hist, width = 11, height = 7, device = "pdf", dpi = 300)

#dD by julian date
deut_by_date <- ggplot(data = na.omit(deut_data), aes(x = Julian, y = dD)) +
  geom_point() +
  geom_smooth(method = "lm")
ggsave("figures/dD_by_date.pdf", plot = deut_by_date, width = 11, height = 7, device = "pdf", dpi = 300)

#mass by date
(mass_by_date <- ggplot(data = morph_data[morph_data$PAge != "",]) +
  geom_point(aes(x = Julian, y = Mass.1, color = PAge)) + 
  geom_smooth(aes(x = Julian, y = Mass.1, color = PAge), method = "lm"))
ggsave("figures/mass_by_date.pdf", plot = mass_by_date, width = 11, height = 7, device = "pdf", dpi = 300)

#violin plot of capture by date
sexknown <- morph_data[morph_data$Final.Sex != "U",]
sexknown <- sexknown[sexknown$PAge == "A",]
sexknown$sexyear <- paste0(sexknown$Final.Sex, sexknown$Year)
(caps_by_sex_date <- ggplot(data = sexknown, aes(x = sexyear, y = Julian, fill = Final.Sex)) +
  geom_violin(alpha = .3) +
  geom_boxplot(width = 0.2))
ggsave("figures/cap_by_date_violin.pdf", plot = caps_by_sex_date, width = 11, height = 7, device = "pdf", dpi = 300)

#mosaic of age and sex
ggplot(data = morph_data[which(morph_data$PAge != ""),]) +
  geom_mosaic(aes(x = product(Final.Sex), fill = Final.Sex, conds = product(PAge)))
ggsave("figures/sex_by_age.pdf", plot = caps_by_sex_date, width = 11, height = 7, device = "pdf", dpi = 300)

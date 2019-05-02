library(ggplot2)
prospecting_iso <- read.csv("owl_dD_syscratch.csv")
str(prospecting_iso)

hist(prospecting_iso$dD[which(prospecting_iso$status == "r")])
abline( v= mean(prospecting_iso$dD[which(prospecting_iso$status == "u")]))

ggplot(data = prospecting_iso, aes(x = dD, group = status)) +
  geom_density(aes(fill = status))

res_only <- prospecting_iso[prospecting_iso$status == "r",]
non_res_only <- prospecting_iso[prospecting_iso$status == "n",]
pros_no_unk <- prospecting_iso[prospecting_iso$status != "u",]

ggplot(data = pros_no_unk, aes(x = dD)) +
  geom_density(aes(fill=status, alpha = .2))

ggplot(data = res_only, aes(x = dD)) +
  geom_density()
     
ggplot(data = non_res_only, aes(x = dD)) +
  geom_density()

#prop of unk birds within range of known residents
length(non_res_only$dD[which(non_res_only$dD >range(res_only$dD)[1] & non_res_only$dD < range(res_only$dD)[2])])/length(non_res_only$dD)


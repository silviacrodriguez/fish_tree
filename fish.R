library(ggtree)
library(ape)
library(ggplot2)
library(deeptime)
library(ggimage)

tree <- read.tree (file.choose())
tree$tip.label<-c("Leucoraja erinacea","Hemiscyllium ocellatum", "Carcharodon carcharias", "Scyliorhinus canicula","Takifugu rubripes", "Oryzias latipes", "Danio rerio", "Polypterus senegalus", "Latimeria chalumnae","Petromyzon marinus")
fish<-ggtree(tree,root.position = -569)
fish<-fish + geom_tiplab(size=5.5)
info<-read.table(file.choose(), h=T)
info_bar<-info
info$Species<-c("Hemiscyllium ocellatum","Danio rerio", "Takifugu rubripes","Petromyzon marinus", "Latimeria chalumnae", "Oryzias latipes", "Polypterus senegalus","Carcharodon carcharias", "Scyliorhinus canicula","Leucoraja erinacea" )
info$Class <- factor(info$Class, levels = c("Actinopterygii", "Elasmobranchii", "Actinistia", "Hyperoartia"))
fish%<+% info + geom_tiplab(aes(image=uid, colour= Class), geom="phylopic", size=0.15, offset=194) + scale_colour_manual(values = c(
  "Actinopterygii" = "#05385C",
  "Elasmobranchii" = "#6691AF",
  "Actinistia" = "#B0ABAB","Hyperoartia" = "#9292B2" )) +labs(colour="Class", size=0.15)+ scale_x_continuous(breaks = seq(-570, 0, 30), labels = -seq(-570, 0, 30), name="Time(MYA)") +theme_tree2() +coord_cartesian(clip = 'off')+theme(legend.position= c(0.11,0.85), legend.text=element_text(size = 16), legend.title = element_text(size = 18),legend.background = element_rect(fill = "white", colour = "black", size = 0.5),
                                                                                                                                                                                                                                          legend.box.background = element_rect(colour = "black", size = 1), plot.margin = margin(3,230,3,3))

info_bar$Species<-c("H. ocellatum","D. rerio", "T. rubripes","P. marinus", "L. chalumnae", "O. latipes", "P. senegalus","C. carcharias", "S. canicula","L. erinacea" )
ggplot(data=info_bar, aes(x=Genome.DNA, y=Species)) +
  geom_bar(stat="identity", fill ="#6691AF")+xlab("NUMT count")
#legend.position= c(1.5,0.5)
# how to get the uid for a list of species
d <- ggimage::phylopic_uid(tree2$tip.label)
d$Coverage <- c(0.24, 0.04, 0.0008, 0.002, 0.005, 0.007, 0.005)


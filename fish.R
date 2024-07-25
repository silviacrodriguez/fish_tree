library(ggtree)
library(ape)
library(ggplot2)
library(deeptime)
library(ggimage)
#upload tree file
tree <- read.tree (file.choose())
# Change the tip labels of the tree
tree$tip.label<-c("Leucoraja erinacea","Hemiscyllium ocellatum", "Carcharodon carcharias", "Scyliorhinus canicula","Takifugu rubripes", "Oryzias latipes", "Danio rerio", "Polypterus senegalus", "Latimeria chalumnae","Petromyzon marinus")
# Use ggtree to accomodate the position of the tree so it starts before the modern era (Ex:-569 Million years ago)
fish<-ggtree(tree,root.position = -569)
# Display the tip of the tree labels
fish<-fish + geom_tiplab(size=5.5)

#Upload a dataframe with information regarding the each species included in the tree
info<-read.table(file.choose(), h=T)
#Make the species name in the dataframe the same as the tree labels. If not done it will not work
info$Species<-c("Hemiscyllium ocellatum","Danio rerio", "Takifugu rubripes","Petromyzon marinus", "Latimeria chalumnae", "Oryzias latipes", "Polypterus senegalus","Carcharodon carcharias", "Scyliorhinus canicula","Leucoraja erinacea" )

info$Class <- factor(info$Class, levels = c("Actinopterygii", "Elasmobranchii", "Actinistia", "Hyperoartia"))
# How to get the uid for a list of species. The uid is the "path" to the retrieve the silhouette of an organism from Phylopic
silhouettes <- ggimage::phylopic_uid(tree$tip.label)
# Add the uid obtained to the dataframe so that each specie has a code

# Put the silhouette of the organism at the tip fo the tree
fish<-fish%<+% info + geom_tiplab(aes(image=uid, colour= Class), geom="phylopic", size=0.15, offset=194)+ scale_colour_manual(values = c("Actinopterygii" = "#05385C","Elasmobranchii" = "#6691AF","Actinistia" = "#B0ABAB","Hyperoartia" = "#9292B2" )) + labs(colour="Class", size=0.15)

#Adjust the time scale and make it visible in the plot
fish<-fish%<+% info + scale_x_continuous(breaks = seq(-570, 0, 30), labels = -seq(-570, 0, 30), name="Time(MYA)")

#Adjust the margins, colors and position of legend
fish<-fish%<+% info +theme_tree2() +coord_cartesian(clip = 'off')+
  theme(legend.position= c(0.11,0.85), legend.text=element_text(size = 16), legend.title = element_text(size = 18),
        legend.background = element_rect(fill = "white", colour = "black", size = 0.5),legend.box.background = element_rect(colour = "black", size = 1), plot.margin = margin(3,230,3,3))
View (fish)

# A normal bar graph
info_bar<-info
info_bar$Species<-c("H. ocellatum","D. rerio", "T. rubripes","P. marinus", "L. chalumnae", "O. latipes", "P. senegalus","C. carcharias", "S. canicula","L. erinacea" )
ggplot(data=info_bar, aes(x=Genome.DNA, y=Species)) +
  geom_bar(stat="identity", fill ="#6691AF")+xlab("NUMT count")



library(dendextend)
install.packages("dendextend")
library(colorspace) # get nice colors
df<-read.csv("signatures.csv")

hc<-df[,3:22]
samples.2<-df[,32]
samples_label<- rev(levels(samples.2))
samples<-df[,2]

#Clustering using Multiscale bootstrap resampling
install.packages("pvclust")
library(pvclust)
t<-t(hc)
colnames(t)<-samples
result <- pvclust(t, method.dist="euclidean",method.hclust="average", 
                                nboot=1000)

result.d<-as.dendrogram(result)
labels_colors(result) <-c("palegreen3","skyblue3","deepskyblue4","firebrick1")[sort_levels_values(
  as.numeric(samples.2)[order.dendrogram(result)])]
labels(result) <- paste(as.character(samples)[order.dendrogram(result)],
                        # "(",labels(clusters),")", 
                        sep = "")
#reduce size of labels
result.d<- set(result.d, "labels_cex", 0.9)
plot(result.d, 
     main = "Hierarchical Clustering of Mutational Signatures", 
     horiz =  FALSE,  nodePar = list(cex = .002))
legend(62,1.05, legend = samples_label, fill = c("firebrick1","deepskyblue4","skyblue","palegreen3"),cex = 0.9,bty="n")
plot(result)
pvrect(result, alpha=0.95)
pvpick(result, alpha = 0.95)

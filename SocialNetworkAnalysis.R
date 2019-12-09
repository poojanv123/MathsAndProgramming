# Mathematics and Programming Skills
# Problem set 1
# October 17th, 2019
rm(list = ls())

# Centralization and small worlds: the Indian villages relationships data
library(network)
library(sna)
library(statnet)
library(ggplot2)
library(corrplot)
library(dplyr)
library(ggnetwork)


# Set up
setwd("E:/Study/Maths and programming/ProblemSet1")

# 1.	Load the data
netdata<-read.csv("data.csv",stringsAsFactors=F,header=F)
hhid <- read.csv("key.csv",stringsAsFactors=F,header=F)
hhid <- array(hhid[,1])
rownames(netdata) <- colnames(netdata) <- hhid
attrdata<-read.csv("hhattributes.csv",stringsAsFactors=F,header=T)
attrdata <- attrdata[which(attrdata$village == 34),]

##make network , add attributes
net<-as.network(netdata,directed = FALSE)
for(i in 1:ncol(attrdata))
  net%v%colnames(attrdata)[i]<-attrdata[,i]
gplot(net, gmode = "graph", main = "Household Relationships Network",
      sub = paste("Number of nodes = ", length(network.vertex.names(net)),"\n", "Number of edges = ", network.edgecount(net),
                  "\n","Density = ", round(gden(net),digit = 4)))
# 2.	Which actors have the highest indegree, outdegree , betweenness, closeness and eigenvector centrality scores? Do these scores correlate? If so, how and interpret the findings.

# Degree centrality
deg <- degree(net, gmode = "graph") # Default: total degree
#ideg <- degree(net, cmode="indegree") # Indegree for MIDs
#odeg <- degree(net, cmode="outdegree") # Outdegree for MIDs

# Betweenness
bet <- betweenness(net, gmode="graph") # Geographic betweenness

# Closeness
closeness2 <- function(x){ # Create an alternate closeness function!
  geo <- 1/geodist(x)$gdist # Get the matrix of 1/geodesic distance
  diag(geo) <- 0 # Define self-ties as 0
  apply(geo, 1, sum) # Return sum(1/geodist) for each vertex
}

clos <- closeness2(net) # Use our new function on contiguity data

# Eigenvector
eigen<-evcent(net)

# Visualise the results as a data frame
results<-data.frame(hhid,deg, bet, clos, eigen) 
for(i in 2:ncol(results))
  net%v%colnames(results)[i] <- results[,i]

# Actor with the highest indegree
par(mfrow=c(1,1))
nf <- layout(matrix(c(1,2,3,4),ncol=2,nrow = 2), widths=c(4,4,4,4), heights=c(4,4,4,4), TRUE) 
dev.new(width)
par(margin(10,10,10,10))
gplot(net,gmode="graph",vertex.cex = log(deg, base = 5),main = "Node size by degree", sub = paste("Highest degree household ID: ", hhid[which.max(results$deg)]))
gplot(net,gmode="graph",vertex.cex = log(bet, base = 5),main = "Node size by betweenness", sub = paste("Highest betweenness household ID: ", hhid[which.max(results$bet)]))
gplot(net,gmode="graph",vertex.cex = log(clos,base = 5),main = "Node size by closeness", sub = paste("Highest closeness household ID: ", hhid[which.max(results$clos)]))
gplot(net,gmode="graph",vertex.cex = log(eigen,  base = 5),main = "Node size by eigenvector centrality", sub = paste("Highest eigenvector centrality household ID: ", hhid[which.max(results$eigen)]))

# Score Correlations
score_cor<-cor(results[,2:5])
colnames(score_cor) <- c('Degree', 'Betweenness', 'Closeness', 'Eigenvector')
row.names(score_cor)<- c('Degree', 'Betweenness', 'Closeness', 'Eigenvector')
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
corrplot(score_cor, method = 'number', order ='AOE', type ='lower', 
         col=col4(100), tl.pos = 'd', tl.col = 'black',label_sig = "r", title = "Centrality Scores Correlation", xlab = "Correlation Coefficient(r)")


# 3.Do these scores correlate with any of the attribute data? Pick an attribute, run correlation tests and interpret.
  
attr_cor<- cor(results[,2:5], as.numeric(get.vertex.attribute(net, "room_no")))
par(mfrow=c(2,2))
#par(mar=c(5,4.1,4.1,2.1))
plot(get.vertex.attribute(net, "room_no"), get.vertex.attribute(net, "deg"), pch = 1,col = "blue", xlab = "Number of rooms", ylab = "Degree", main = paste("r = ",round(attr_cor[1],digits = 4)),font.main = 1, cex.main = 1)
plot(get.vertex.attribute(net, "room_no"), get.vertex.attribute(net, "bet"), pch = 3,col = "blue",  xlab = "Number of rooms", ylab = "Betweeness", main = paste("r = ",round(attr_cor[2],digits = 4)),font.main = 1, cex.main = 1)
plot(get.vertex.attribute(net, "room_no"), get.vertex.attribute(net, "clos"),pch = 4, col = "blue",  xlab = "Number of rooms", ylab = "Closeness", main = paste("r = ",round(attr_cor[3],digits = 4)),font.main = 1, cex.main = 1)
plot(get.vertex.attribute(net, "room_no"), get.vertex.attribute(net, "eigen"), pch = 16, col = "blue", xlab = "Number of rooms", ylab = "Eigenvector", main = paste("r = ",round(attr_cor[4],digits = 4)),font.main = 1, cex.main = 1)
mtext("Centrality Scores vs Attribute Correlation", side = 3, line = -1.5, outer = TRUE)


#plot(as.factor(attrdata$castesubcaste), ideg)

# 4.	Is this network a small world? Why or why not? What does that mean about the data? Generate the small world plot from random graphs as discussed in lecture 2. Plot your network on top of that plot. (HINT:  rgraph() generates random graphs with certain paramenters. rgws() generates graphs with different re-wiring parameters. triad.census() calculates your triad census…
# Lattice
lat<-rgws(1,165,1,2,0, return.as.edgelist = FALSE)
lat <- as.network(lat)
trilat<-triad.census(lat, mode='graph')
Llat<-max(geodist(lat, inf.replace=0)$gdist)
cclat <- trilat[4]/(trilat[3] + 3*(trilat[4]))

#P Sequence
p <- 10^(seq(-3,0,0.2))
#p<-c(0, 0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
#p = seq(0,1,0.05)


#gden(net)

netrw <- list()
val <- matrix(data = NA, ncol = 3, nrow = length(p))
lrw <- list()
ccrw <- list()
lnorm <- list()
ccnorm <- list ()
set.seed(20000)
for (j in 1:length(p)) {
  for(i in 1:100) {
    netrw[[i]] <- rgws(1,165,1,2,p[j],return.as.edgelist = FALSE)
    trirgws<-triad.census(netrw[[i]], mode='graph')
    lrw[[i]] <-max(geodist(netrw[[i]], inf.replace=0)$gdist)
    ccrw[[i]] <- trirgws[4]/(trirgws[3] + 3*(trirgws[4]))
    lnorm[[i]] <- lrw[[i]]/Llat
    ccnorm[[i]] <- ccrw[[i]]/cclat
  }
  val[j,1] <- p[j]
  val[j,2] <- mean(unlist(lnorm))
  val[j,3] <- mean(unlist(ccnorm))
}

df1 <- data.frame(p,"y" = val[,2], "var" = "Lp/L0")
df2 <- data.frame(p,"y" = val[,3], "var" = "Cp/C0")
resultdf <- rbind(df1,df2) 
ggplot(resultdf, aes(x=p, y=y, color=var)) + 
  labs(title = "Small World plot", y ="", color = "") + 
  geom_point() + scale_x_log10() + theme(plot.title = element_text(hjust = 0.5))


# Net length and clustering
trinet<-triad.census(net, mode='graph')
Lnet<-max(geodist(net, inf.replace=0)$gdist)
ccnet <- trinet[4]/(trinet[3] + 3*(trinet[4]))

# Random
ran <- rgraph(165, m=1, tprob=gden(net,mode = "graph"),mode = "graph")
ran <- as.network(ran)
triran<-triad.census(ran, mode='graph')
Lran<-max(geodist(ran, inf.replace=0)$gdist)
ccran <- triran[4]/(triran[3] + 3*(triran[4]))
smallw <- c(Lnet, Lran, ccnet,ccran)
smallw
par(mfrow=c(1,3))
plot(lat, main = "Regular")
plot(net, main = "Small World")
plot(ran, main = "Random")

#Question 5
library(sqldf)
#independent nodes vs caste
#boxplot(get.vertex.attribute(net,"castesubcaste")[nonode])
 # nonode <- as.numeric(which(as.numeric(get.vertex.attribute(net, "ideg"))) == 0)
#hhid[which(ideg == 0)]
p1 <- data.frame(net%v%"castesubcaste",net%v%"ideg")
colnames(p1) <- c("caste", "ideg")
pdata <- aggregate(p1$ideg,list(p1$caste),mean)
plot(pdata$Group.1,pdata$x)

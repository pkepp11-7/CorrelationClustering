install.packages("igraph")
install.packages("ISLR")
library(ISLR)
library(igraph)


#####################################Correlation Clustering ALgorithm#########################################
CorrelationCluster = function(Vin, Ein)  {
  Vcurrent = Vin
  Vnext = vector()
  clusters = list()
  count = 1
  len = length(Vcurrent)
  #while there are more potential clusters
  while(len > 1)
  {
    cluster = vector()
    #pick a pivot
    pivot = sample(Vcurrent, 1)
    cluster = c(cluster, pivot)
    Vcurrent=Vcurrent[-which(Vcurrent==pivot)]
    for (j in Vcurrent)
    {
      if(Ein[pivot,j] == 1)
      {
        cluster = c(cluster, j)
      }
      else
      {
        Vnext = c(Vnext, j)
      }
    }
    Vcurrent = Vnext
    len = length(Vcurrent)
    Vnext = vector()
    clusters[[count]] = sort(cluster)
    count = count + 1
  }
  if(len == 1)
  {
    clusters[[count]] = Vcurrent
  }
  return(clusters)
}



####################Application 1: +-- Balanced Triad###########################
signed_adj_matrix = matrix(nrow = 3, ncol = 3)

signed_adj_matrix[1,2] = signed_adj_matrix[2,1] = 1
signed_adj_matrix[1,3] = signed_adj_matrix[3,1] = -1
signed_adj_matrix[2,3] = signed_adj_matrix[3,2] = -1

cc_out = CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)


###################Application 2: K-Means Clustering Combination################

#create randomized test data
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4



#convert to signed adjacency matrix ([i,j] = 1 if point i and j belong to same cluster)
initMatrix = function(n)
{
  x = matrix(nrow=n, ncol=n)

  for(i in 1:n)
  {
    for(j in 1:n)
    {
        x[i,j] = 0
    }
  }
  return(x)
}





computeSimilarity = function(x, adj_mat)
{
  for(i in 1:nrow(adj_mat))
  {
    for(j in i:nrow(adj_mat))
    {
      if(x[i] == x[j])
      {
        adj_mat[i,j] = adj_mat[i, j] + 1
        adj_mat[j,i] = adj_mat[j, i] + 1
      }
      else
      {
        adj_mat[i,j] = adj_mat[i,j] - 1
        adj_mat[j,i] = adj_mat[j,i]  - 1
      }
    }
  }
  return(adj_mat)
}

#perform k-means clustering
signed_adj_matrix=initMatrix(nrow(x))
km.out = kmeans(x, 2, nstart=20)
signed_adj_matrix = computeSimilarity(km.out$cluster, signed_adj_matrix)

cc_out = CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)
cc_out[[1]]
cc_out[[2]]

#perform k-means clustering with 3 clusters
km.out = kmeans(x, 3, nstart=20)
signed_adj_matrix = computeSimilarity(km.out$cluster, signed_adj_matrix)

#kmeans clustering with 4 clusters
km.out = kmeans(x, 4, nstart=20)
signed_adj_matrix = computeSimilarity(km.out$cluster, signed_adj_matrix)

#kmeans clustering with 5 clusters
km.out = kmeans(x, 5, nstart=20)
signed_adj_matrix = computeSimilarity(km.out$cluster, signed_adj_matrix)

#kmeans clustering with 6 clusters
km.out = kmeans(x, 6, nstart=20)
signed_adj_matrix = computeSimilarity(km.out$cluster, signed_adj_matrix)

#get signed network from majority vote
similar = which(signed_adj_matrix > 0)
signed_adj_matrix[similar] = 1
signed_adj_matrix[-similar] = -1
cc_out2 = CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)


###################Application 3: Feature-By-Feature Similarity################

View(Carseats)

meanDistance = function(x) 
{
  dist = 0
  len = length(x)
  for(i in 1:len)
  {
    for(j in (i:len)[-(1:i)])
    {
      dist = dist + (abs(x[j]-x[i])/(len * (len-1)/2))
    }
  }
  return(dist)
}

sales_dist = meanDistance(Carseats$Sales)
compprice_dist = meanDistance(Carseats$CompPrice)
income_dist = meanDistance(Carseats$Income)
advertising_dist = meanDistance(Carseats$Advertising)
population_dist = meanDistance(Carseats$Population)
price_dist = meanDistance(Carseats$Price)
age_dist = meanDistance(Carseats$Age)

signed_adj_matrix = initMatrix(nrow(Carseats))

for(i in 1:nrow(Carseats))
{
  for(j in (1:nrow(Carseats))[-(1:i)])
  {
    if(abs(Carseats$Sales[j] - Carseats$Sales[i]) < sales_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$CompPrice[j] - Carseats$CompPrice[i]) < compprice_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$Income[j] - Carseats$Income[i]) < income_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$Advertising[j] - Carseats$Advertising[i]) < advertising_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$Population[j] - Carseats$Population[i]) < population_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$Price[j] - Carseats$Price[i]) < price_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    if(abs(Carseats$Age[j] - Carseats$Age[i]) < age_dist)
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] + 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] + 1
    }
    else
    {
      signed_adj_matrix[i, j]   = signed_adj_matrix[i, j] - 1
      signed_adj_matrix[j, i]   = signed_adj_matrix[j, i] - 1 
    }
    
  }
}

similar = which(signed_adj_matrix > 0)
signed_adj_matrix[similar] = 1
signed_adj_matrix[-similar] = -1

cc_out3 = CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)

cc_out3

###################Application 4: Principal Component Similarity################

numeric = c("Sales", "CompPrice", "Income", "Advertising", "Population", "Price", "Age")
pr.out = prcomp(Carseats[,numeric], scale=TRUE)
pc_dist=unname(meanDistance(pr.out$x[,"PC1"]))
pc_dist=c(pc_dist, unname(meanDistance(pr.out$x[,"PC2"])))
pc_dist=c(pc_dist, unname(meanDistance(pr.out$x[,"PC3"])))
pc_dist=c(pc_dist, unname(meanDistance(pr.out$x[,"PC4"])))
pc_dist=c(pc_dist, unname(meanDistance(pr.out$x[,"PC5"])))
pc_dist=c(pc_dist, unname(meanDistance(pr.out$x[,"PC6"])))
pc_dist=c(pc_dist, uname(meanDistance(pr.out$x[,"PC7"])))

pr.variance = pr.out$sdev^2
pve = pr.variance/sum(pr.variance)
signed_adj_matrix = initMatrix(nrow(pr.out$x))

length(pc_dist)

for(i in 1:nrow(pr.out$x))
{
  for(j in ((1:nrow(pr.out$x))[-(1:i)]))
  {
    for(pc in 1:length(pc_dist))
    {
      if(abs(pr.out$x[j,pc] - pr.out$x[i,pc]) < pc_dist[pc])
      {
        signed_adj_matrix[i,j] = signed_adj_matrix[i,j] + pve[pc]
        signed_adj_matrix[j,i] = signed_adj_matrix[i,j]
      }
      else
      {
        signed_adj_matrix[i,j] = signed_adj_matrix[i,j] - pve[pc]
        signed_adj_matrix[j,i] = signed_adj_matrix[i,j]
      }
    }
  }
}

length(which(signed_adj_matrix == 0))

similar = which(signed_adj_matrix > 0)
signed_adj_matrix[similar] = 1
signed_adj_matrix[-similar] = -1

cc_out4= CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)


###################Application 4: Network Cluster Combination################

KarateClub = read.graph("karate.gml", "gml")
gn_cluster = cluster_edge_betweenness(KarateClub, directed=is_directed(KarateClub))
membership(gn_cluster)

lv_cluster= cluster_louvain(KarateClub)
membership(lv_cluster)

prop_cluster = cluster_label_prop(KarateClub)
membership(prop_cluster)


signed_adj_matrix = initMatrix(length(V(KarateClub)))
signed_adj_matrix = computeSimilarity(membership(gn_cluster), signed_adj_matrix)
signed_adj_matrix = computeSimilarity(membership(lv_cluster), signed_adj_matrix)
signed_adj_matrix = computeSimilarity(membership(prop_cluster), signed_adj_matrix)

similar = which(signed_adj_matrix > 0)
signed_adj_matrix[similar] = 1
signed_adj_matrix[-similar] = -1

cc_out5 = CorrelationCluster(1:nrow(signed_adj_matrix), signed_adj_matrix)

cc_membership = vector(length = length(V(KarateClub)))
cc_out5[[1]]
for(i in 1:length(cc_out5))
{
  x = cc_out5[[i]]
  for(j in 1:length(x))
  {
    cc_membership[x[j]] = i
  }
}
View(cc_out5)

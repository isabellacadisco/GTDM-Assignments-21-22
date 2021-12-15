library(igraph)
library(Matrix)
source("mysimrank.R")
g<-read_graph("/Users/andrewcosta/Desktop/Milan/Maths/graph4.gml",format=c("gml"))
plot.igraph(g)
adjmat<-as_adj(g)
vertex_attr(g)

v1<-induced_subgraph(g,1:20)
vertex_attr(v1)
plot.igraph(v1)
adjmat1<-as_adj(v1)
adjmat1 
s1<-similarity(v1, method= "dice")
s1

v2<-induced_subgraph(g,21:50)
vertex_attr(v2)
plot.igraph(v2)
adjmat2<-as_adj(v2)
adjmat2
s2<-similarity(v2, method= "dice")
s2

v3<-induced_subgraph(g,51:70)
vertex_attr(v3)
plot.igraph(v3)
adjmat3<-as_adj(v3)
adjmat3
s3<-similarity(v3, method= "dice")
s3

fakenews <-matrix(0,50,1)

infection_1 <- matrix(0,70,1)
infection_2 <- matrix(0,70,1)

person_zero <-sample(v1,1)
person_zero

infection_1[person_zero] <-1
infection_2[person_zero] <-1
infection_1

for(t in 1:50)
{
  for(j in 1:70)
  {
    if(infection_1[j] == 1)
    {
      for(i in 1:70)
      {
        if(adjmat[i,j]==1)
        {
          if(infection_1[i]==0)
          {
            infected <-0
            for(k in 1:70)
            {
              if(adjmat[k,i] == 1)
              {
                if(infection_1[k]==1)
                {
                  infected <- infected+1
                }
              }
            }
            if(infected >5)
            {
              infection_2[i] = 1
            }
            else
            {
              transfer <-runif(1,0,1)
              if(transfer <= 0.2)
              {
                infection_2[i]=1
              }
            }
          }
        }
      }
    }
  }
  infection_1 <- infection_2
  print("time_steps")
  print(t)
  print(sum(infection_1))
  fakenews[t]<-sum(infection_1)
}
plot((fakenews), type='l')

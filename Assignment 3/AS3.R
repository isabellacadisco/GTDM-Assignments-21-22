InfluenceStates<-c('Susceptible', 'Infected', 'Recovered')
Statestransition<-matrix(c(0.92, 0.08, 0, 0, 0.4, 0.6,
                           0.9, 0, 0.1 ),nrow=3,ncol=3,byrow=TRUE)
dimnames=list(InfluenceStates,InfluenceStates)
Statestransition
library(markovchain)
Mcstates<-new("markovchain",states=InfluenceStates,byrow=T,transitionMatrix=Statestransition,name="individuals' changing")
Mcstates
library(diagram)
rownames(Statestransition) <- InfluenceStates
colnames(Statestransition) <- InfluenceStates
plotmat(Statestransition,box.col ="lightblue")

Mcstates^2
Mcstates^3
Mcstates^50
Mcstates^150
Mcstates^10000
steadyStates(Mcstates)

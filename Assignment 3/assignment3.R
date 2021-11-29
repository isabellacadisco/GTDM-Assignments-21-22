library ( ggplot2 )

#linear system solution

S<-c('Susceptible', 'Infected', 'Removed')

P<-matrix(c(0.92, 0.08, 0, 
            0, 0.4, 0.6,
            0.1, 0, 0.9 ),nrow=3,ncol=3,byrow=TRUE)


A <- matrix(c(-0.08, 0.08, 0, 1, 0, -0.6, 0.6, 1, 0.1, 0, -0.1, 1 ), 
                   ncol=3,nrow=4)
A

b <- c(0,0,0, 1)

pi <- qr.solve(A,b)
names(pi) <- c('Susceptible', 'Infected', 'Removed')
pi 



#VISUALIZATION

#initial distribution
m_0 <- c(1,0,0)

n_it <- 150

evolution <- matrix(nrow = n_it, ncol = 3)

P_n = P

evolution[1,] = m_0


#compute n_it iterations
for(n in c(2:n_it)){
  evolution[n,] = m_0%*%P_n
  P_n = P_n%*%P
}

#plot

evolution_df = as.data.frame(evolution)
names(evolution_df) = S

ggplot() +
  ylim(0, 1) +
  geom_line(data = evolution_df, aes(x = c(1:n_it),
                                     y = Susceptible, color =
                                       "Susceptible")) +
  geom_line(data = evolution_df, aes(x = c(1:n_it),
                                     y = Infected, color =
                                       "Infected")) +
  geom_line(data = evolution_df, aes(x = c(1:n_it),
                                     y = Removed, color =
                                       "Removed")) +
  labs(colour="States", x="iterations", y="probability distribution") + 
  theme(plot.title = element_text(hjust = 0.5))
  
  
results = evolution[n_it,]
names(results) = S
results


#probability distribution starting from a different inital distribution--------

m_1 <- c(0.3,0.7,0)

evolution_1 <- matrix(nrow = n_it, ncol = 3)

P_n_1 = P

evolution_1[1,] = m_1


#compute n_it iterations
for(n in c(2:n_it)){
  evolution_1[n,] = m_1%*%P_n_1
  P_n_1 = P_n_1%*%P
}

#plot

evolution_1_df = as.data.frame(evolution_1)
names(evolution_1_df) = S

ggplot() +
  
  ylim(0, 1) +
  geom_line(data = evolution_1_df, aes(x = c(1:n_it),
                                     y = Susceptible, color =
                                       "Susceptible")) +
  geom_line(data = evolution_1_df, aes(x = c(1:n_it),
                                     y = Infected, color =
                                       "Infected")) +
  geom_line(data = evolution_1_df, aes(x = c(1:n_it),
                                     y = Removed, color =
                                       "Removed")) +
  labs(colour="States", x="iterations", y="probability distribution") +
  theme(plot.title = element_text(hjust = 0.5))


results_1 = evolution_1_df[n_it,]
names(results_1) = S
results_1
#-----------------------------------------

#total variation
tot_var = function(v1,v2){
  sum(abs(v1-v2))/2
}

pi
results

paste('tot var naive - lin sys: ',
      tot_var(results, pi))

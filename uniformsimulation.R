#Function for simulating uniform(7, 13) random variables and determining the proportion greater than or equal to 9
# n : sample size
# print : TRUE/FALSE for printing vector with observations and true/false-ness

simulation<-function(n, print)
{
  #Generate n random uniform observations on (7,13)
  obs<-runif(n, 7, 13)
  
  #If print=T, print vector of observations and vector of T/F values
  if(print==T){
    print(obs)
    print((obs>=9)==TRUE)
  }
  
  #Count proportion where >=9 is true out of n observations
  sum((obs>=9&obs<=13)==TRUE)/n
  
}

#Test for larger sample sizes
simulation(10,T)
simulation(100,F)
simulation(10000,F)
simulation(100000,F)
simulation(1000000,F)

#Create an empty numeric vector
prop<-vector('numeric')

#Create a vector with 1000 instances of function with sample size increasing by 100
for(k in 1:1000){
  
  prop<-c(prop,simulation(100*k,F))
  
}
 
#Create a vector of possible sample sizes
num<-c(1:1000)
num<-num*100

#Plot sample size vs proportion, "l" option says to plot using a line
plot(num, prop, type="l")


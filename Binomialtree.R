Binomialtreeforoptions<-function(TypeFlag = c("EC","EP","AC","AP"), S0, K, T, r, sigma, N) {

  #constants for binomial calculation
  dt = T/N
  nu = r - 0.5*sigma^2
  dxu = sqrt(sigma^2*dt+(nu*dt)^2)
  dxd = -dxu
  pup = 0.5+0.5*(nu*dt/dxu)
  pdown = 1-pup
  M=N+1
  
  #discount factor & constants
  disc = exp(-r*dt)
  dpup = disc*pup
  dpdown = disc*pdown
  edxud = exp(dxu-dxd)
  edxd = exp(dxd)
  St = ValueofOption = 0
  St[1] = S0*exp(N*dxd)

  
  #for European Call Option
  #asset price at N
  if(TypeFlag=="EC"){
    for (j in 2:M){
      St[j]=St[j-1]*edxud            #Asset price at maturity
    }
    
    for (j in 1:M){
      ValueofOption[j] = max(0,St[j]-K)     #Option value at maturity
    }
    for (j in seq(from=M-1, to=1, by=-1)){
      for (i in 1:j){
        ValueofOption[i] = disc*(pup*ValueofOption[i+1]+pdown*ValueofOption[i])
      }
    }
    } else if (TypeFlag=="EP"){
    for (j in 2:M){
      St[j]=St[j-1]*edxud            #Asset price at maturity
    }
    for (j in 1:M){
      ValueofOption[j] = max(0,K-St[j])     #Option value at maturity
    }
    for (j in seq(from=M-1, to=1, by=-1)){   #Step back through tree
      for (i in 1:j){
        ValueofOption[i] = disc*(pup*ValueofOption[i+1]+pdown*ValueofOption[i])
      }
    }
    } else if (TypeFlag=="AC"){
      for (j in 2:M){
        St[j] = St[j-1]*edxud       #Asset price at maturity
      }

      for (j in 1:M){
        ValueofOption[j]=max(0,St[j]-K)      #Option value at maturity
      }
      for (j in seq(from=M-1,to=1,by=-1)){
        for (i in 1:j){
          ValueofOption[i]=dpdown*ValueofOption[i]+dpup*ValueofOption[i+1]    #Step back through tree
          St[i]=St[i]/edxd     #adjust price to current time step
          ValueofOption[i]=max(ValueofOption[i],St[i]-K)
        }
      }
    } else if (TypeFlag=="AP"){
      for (j in 2:M){
        St[j] = St[j-1]*edxud       #Asset price at maturity
      }

      for (j in 1:M){
        ValueofOption[j]=max(0,K-St[j])      #Option value at maturity
      }
      for (j in seq(from=M-1,to=1,by=-1)){
        for (i in 0:j){
          ValueofOption[i]=dpdown*ValueofOption[i]+dpup*ValueofOption[i+1]    #Step back through tree
          St[i]=St[i]/edxd     #adjust price to current time step
          ValueofOption[i]=max(ValueofOption[i],K-St[i])
        }
      }
    }

(ValueofOption[1])
}

#there might be R documentation to execute some of these methods 

#mean
#mean(c(a,b)) returns the mean from a vector 

#sum(c(a,b)) returns the sum of a vector 

#note, you can use var(), sdev() and mean() instead of the following functions. 
#these functions below are more situational . 

#expected Value
e_val = function(x, px){
  return(sum(x * px))
}

#variance
variance = function(x, px){
  return(sum((x - e_val(x,px))^2 * px))
}

#sdev 
sdev = function(x,px){
  return(sqrt(var(x,px)))
}


#binomial commands 
#dbinom(success, n, p) -> returns probability where (x = n)
#pbinom(success, n, p) -> returns aggregate probability where (x < n)
#pbinom(success, n, p, lower.tail = FALSE) -> returns aggregate where (x > n)

#acts like binom but include deliminates between x <= 5 or x < 5
#display_binom_pdf(sucess, n, p)
display_binom_pdf = function(success, n, p, lower.tail = TRUE, include = TRUE ){
  if(include){
    if(lower.tail){
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PDF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success + 1 ), rep("gray", n-success )))}
    else{
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PDF", xlab = 'X', ylab = 'Probability',col = c(rep("gray",success  ), rep("blue", n-success )))  
    }
  }else{
    if(lower.tail){
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PDF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success ), rep("gray", n-success )))}
    else{
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PDF", xlab = 'X', ylab = 'Probability',col = c(rep("gray",success + 1 ), rep("blue", n-success-1 )))  
    }  
  }
}


#acts like binom but include deliminates between x <= 5 or x < 5
display_binom_cdf = function(success, n, p, lower.tail = TRUE, include = TRUE ){
  if(include){
    if(lower.tail){
      barplot(height = pbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial CDF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success + 1 ), rep("gray", (n - success) )))}
    else{
      barplot(height = pbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial CDF", xlab = 'X', ylab = 'Probability',col = c(rep("grey", success), rep("blue", (n - success) - 1)))  
    }
  }else{
    if(lower.tail){
      barplot(height = pbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial CDF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success), rep("gray", (n - success) + 1)))}
    else{
      barplot(height = pbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial CDF", xlab = 'X', ylab = 'Probability',col = c(rep("grey", success + 1) , rep("blue", (n - success) + 1)))  
    }
  }
}

#Hypergeometric Distribution. Yes I know there are R commands but it's very confusing. 
hyper = function(k, x, n, N){
  return(sum((choose(k,x)*choose(N - k,n - x))/choose(N,n)))
}

#poisson commands
#dpois(n, lambda) -> returns probability where (x = n)
#ppois(n, lambda) -> returns aggregate probability where (x < n)
#ppois(n, lambda, lower.tail = FALSE) -> returns aggregate probability where (x > n)

#anonomous functions.  
#use sapply(vector_set, function(x) [YOUR FUNCTION HERE])
#Example sapply(c(1,2), function(x) 800 * x - 900) to compute functions like 800x - 900
# where c = 1, 2, 3 ect. 

#Compute CDF with anonymous function. Example: cdf(c(a,b), function(x) x^2) = b^2 - a^2
#Basically is F(b) - F(a) 
cdf = function(arr, cdf_function){
  if(length(arr) > 2) print("Warning: more than 2 values used!");
  result = sapply(rev(arr), cdf_function)
  out = 2*result[1]
  for(i in result){
    out = out - i
  } 
  return(out)
}

#generate zScore
zscore = function(X,mu,sdev){
  return((X - mu) / sdev)
}

#Generate a high and low value based on following data
gen_range = function(n, e_val, var, p){
  values = c(qnorm((1-p)/2), -1 * qnorm((1-p)/2) )
  return(sapply(rev(values), function(x) x * sqrt(var/n) + e_val  ))
}

#generate probability based of a high and low value
rev_range = function(n, e_val, var, vals){
  values = pnorm(vals, e_val, sqrt(var/n))
  return(values[2] - values[1] )
}

#cacluate probability from X, mu, sdev directly
#pnorm(N, mean, sd) calculate Lower tail of distribution
#pnorm(N, mean, sd, lower.tail = FALSE) calculate upper tail of distribution 


#Turn probability from normal distribution back into zscore
#qnorm(probability) where 0 <= probability <= 1

#Find the probability that women are between 1.40 and 1.84 meters tall with mean = 1.62
#and sdev = .11
#cdf(c(1.4,1.84), function(x) pnorm(x, mean = 1.62, sd = 0.11) )

#exponetial Distribution
#pexp(p_x, beta^-1) calculate lower tail.
#pexp(p_x, beta^-1, lower.tail = FALSE) calculate upper tail.

#beta Distribution 
#pbeta(p_x, alpha, beta) calculate lower tail
#pbeta(p_x, alpha, beta, lower.tail = FALSE) calculate upper tail

motivation = function(){
  val = sample(1:3,1)
  if(val == 1){ print("You got this!")}
  else if(val == 2){print("Be the change you want to see in the world!")}
  else{print("You're doing great sweety!") }
} 



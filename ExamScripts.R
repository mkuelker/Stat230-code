#there might be R documentation to execute some of these methods 

#mean
#mean(c(a,b)) returns the mean from a vector 

#sum(c(a,b)) returns the sum of a vector 

#note, you can use var(), sdev() and mean() instead of the following functions. 
#these functions below are more situational for expected values and variances

#formulasheet
formula_sheet = function(){
  browseURL("https://canvas.ubc.ca/courses/81976/files/18240939/download?download_frd=1")
}

#faster way to use the round function. Just change n from default
r = function(value, n = 4){
  return(round(value, n))
}

#perms
perm = function(x, y){
  return(factorial(x) / factorial(x-y))
}


#words("examplestring", numberofletterwords, repeats
words = function(str, n, repeats = FALSE){
  
  #turn string into vector 
  library(stringr)
  vecstring = c()
  len = str_count(str) 
  for(i in c(1:len)){
    vecstring[i] = substring(str, i, i)
  }
  
  #create string of only uniques 
  idx = 1
  unique = c()
  for(i in vecstring){
    if(!(i %in% unique)){
      unique[idx] = i;
      idx = idx + 1
    }
  }
  
  #return vector of how many times letters repeat 
  ucount = length(unique)
  idx = 1
  count = c()
  for(i in unique){
    count[i] = sum(i == vecstring)
    idx = idx + 1
  }
  
  print(count)
  
  #statement for repetitions 
  if(ucount == len){
    if(repeats){
      return(ucount^n)
    }else{
      
      return(perm(len, n))
    }
  }else{
    if(repeats){
      
      return(ucount^n)
    }else{
      return(factorial(len)/prod(factorial(count)))
    }
  }
}

#expected Value
eval = function(x, px){
  return(sum(x * px))
}

#variance
variance = function(x, px){
  return(sum((x - eval(x,px))^2 * px))
}

#sdev 
sdev = function(x,px){
  return(sqrt(variance(x,px)))
}

#binomial commands 
#dbinom(success, n, p) -> returns probability where (x = n)
#pbinom(success, n, p) -> returns aggregate probability where (x < n)
#pbinom(success, n, p, lower.tail = FALSE) -> returns aggregate where (x > n)

#dhyper(x,m,n,k)
#x : the value(s) of the variable, (amount of hearts we want in hand of 5)
#m : the number of success in the population, (hearts in deck of cards)
#n : the number of failure in the population, (everything not hearts)
#k : the sample size selected from the population. (hand draw)

#poisson commands
#dpois(n, lambda) -> returns probability where (x = n)
#ppois(n, lambda) -> returns aggregate probability where (x < n)
#ppois(n, lambda, lower.tail = FALSE) -> returns aggregate probability where (x > n)

#exponetial Distribution
#pexp(p_x, beta^-1) calculate lower tail.
#pexp(p_x, beta^-1, lower.tail = FALSE) calculate upper tail.

#beta Distribution 
#pbeta(p_x, alpha, beta) calculate lower tail
#pbeta(p_x, alpha, beta, lower.tail = FALSE) calculate upper tail

#acts like binom but include deliminates between x <= 5 or x < 5
#display_binom_pdf(sucess, n, p)
display_binom_pmf = function(success, n, p, lower.tail = TRUE, include = TRUE ){
  if(include){
    if(lower.tail){
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PMF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success + 1 ), rep("gray", n-success )))}
    else{
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PMF", xlab = 'X', ylab = 'Probability',col = c(rep("gray",success  ), rep("blue", n-success )))  
    }
  }else{
    if(lower.tail){
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PMF", xlab = 'X', ylab = 'Probability',col = c(rep("blue", success ), rep("gray", n-success )))}
    else{
      barplot(height = dbinom(0:n, size = n, p), names.arg = 0:n, ylim = c(0,1),main = "Binomial PMF", xlab = 'X', ylab = 'Probability',col = c(rep("gray",success + 1 ), rep("blue", n-success-1 )))  
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
zscore = function(x,mean,sd){
  return((x - mean) / sd)
}

#Generate a CI based on following data
zrange = function(n, mean, var, p){
  values = c(qnorm((1-p)/2), -1 * qnorm((1-p)/2) )
  return(sapply(rev(values), function(x) x * sqrt(var/n) + mean))
}

#generate probability value based ci
zrev_range = function(n, mean, var, vals){
  values = pnorm(vals, mean, sqrt(var/n))
  return(values[2] - values[1])
}

#cacluate probability from X, mu, sdev directly
#pnorm(x, mean, sd) calculate Lower tail of distribution
#pnorm(x, mean, sd, lower.tail = FALSE) calculate upper tail of distribution 

#Turn probability from normal distribution back into zscore
#qnorm(probability) where 0 <= probability <= 1


conf.interval = function(p, x, sd){
  p = (1 - p)/2 #compute alpha  
  arr = c(-1*qnorm(p), qnorm(p)) * sd
  return(round(x - arr, 4))
}

t.interval = function(p, x, sd, n){
  p = (1 - p)/2 #compute alpha  
  arr = c(-1*qt(p,n-1 ), qt(p,n-1)) * sd/sqrt(n)
  return(x - arr)
}

interval_range = function(interval){
  out = 2 * interval[2]
  for(i in interval){
    out = out - i
  }
  return(out)
}

n_find = function(p,sd, m){
  p = abs(qnorm((1 - p)/2)) # Calculate alpha and find zscore
  out = ceiling(((sd * p )/ m)^2) #general formula and round up
  if(out < 30 ){print("Warning, unreliable sample size! n < 30")}
  return(out) 
}


#ttest = function(x, mean, sd, n, alternative = "two.sided", alpha = 0.05){



ttest = function(x, mean, sd, n, alternative = "two.sided", alpha = 0.05){
  m = sd/sqrt(n)
  t = (x - mean)/m
  df = n - 1
  t_shell(x, mean, m, t, df, alternative, alpha)
}

ztest = function(x, mean, sd, n, alternative = "two.sided", alpha = 0.05){
  m = sd/sqrt(n)
  t = (x - mean)/m
  df = n 
 
  
  print("Z-Test")
  crit = 0
  h1 = "";
  pval = 0
  
  if(alternative == "two.sided"){
    if(t >= 0){
      pval = 2 * pnorm(t, lower.tail=FALSE)
    
    }else{
      pval = 2 * pnorm(t, lower.tail=TRUE)
    }
    crit = qnorm(1 - alpha/2)
    h1 = paste("alternative hypothesis: true mean is not equal to", mean)
  }else if(alternative == "less"){
    pval = pnorm(t, lower.tail=TRUE)
    crit = qnorm(alpha)
    h1 = paste("alternative hypothesis: true mean is less than", mean)
  }else if(alternative == "greater") {
    crit = qnorm(1 - alpha)
    pval = pnorm(t, lower.tail=FALSE)
    h1 = paste("alternative hypothesis: true mean is greater than", mean)
  }else{
    print("error, check test value string")
  }
  
  print(sprintf("Z = %.4f, df = %.4f, critical = %.4f and %.4f", t, df, -1 * crit,  crit) )
  print(h1)
  print(paste("sample mean of x:", x ))
  print(sprintf("pval: %.6f, alpha = %.4f", pval, alpha ))
  if(alternative == "two.sided"){
    conf = c(x - crit * m, x + crit * m)
    print(sprintf("%.3f percent confidence interval: %.4f, %.4f", 1 - alpha, conf[1], conf[2]))
  }
  if(pval > alpha){
    print("fail to reject Ho")
  }else{
    print("reject Ho")
  }
}

i_ttest = function(x, y, sd_x, sd_y, n_x, n_y, sigma_equal = TRUE, alternative = "two.sided", alpha = 0.05){
  x = x - y
  mean = 0
  
  if(sigma_equal){
    df = (n_x + n_y) - 2
    sp2 = ( (n_x - 1) * sd_x^2 + (n_y - 1) *sd_y^2  ) /df
    m = sqrt((sp2/n_x) +(sp2/n_y))
    t = (x - mean)/m
    df = (n_x + n_y) - 2
    t_shell(x, mean, m, t, df, alternative, alpha)
  }else{
    m = sqrt( (sd_x^2/n_x) + (sd_y^2/n_y) )
    t = (x - mean)/m
    df = welch(sd_x, sd_y, n_x, n_y)
    t_shell(x, mean, m, t, df, alternative, alpha)
  }
}

#this is pulled from the announcment. Hopefully this code works
welch = function(sa, sb, na, nb){
  numerator = (sa^2/na + sb^2/nb)^2
  denom = (sa^2/na)^2/(na-1) + (sb^2/nb)^2/(nb-1)
  return(numerator/denom)
}

t_shell = function(x, mean, m, t, df, alternative = "two.sided", alpha = 0.05){
  print("T-Test")

  crit = 0
  h1 = "";
  pval = 0
  
  
  if(alternative == "two.sided"){
    if(t >= 0){
      pval = 2 * pt(t, df, lower.tail=FALSE)
    }else{
      pval = 2 * pt(t, df, lower.tail=TRUE)
    }
    crit = qt(1 - alpha/2, df)
    h1 = paste("alternative hypothesis: true mean is not equal to", mean)
  }else if(alternative == "less"){
    pval = pt(t, df, lower.tail=TRUE)
    crit = qt(alpha, df)
    h1 = paste("alternative hypothesis: true mean is less than", mean)
  }else if(alternative == "greater") {
    crit = qt(1 - alpha, df)
    pval = pt(t, df, lower.tail=FALSE)
    h1 = paste("alternative hypothesis: true mean is greater than", mean)
  }else{
    print("error, check test value string")
  }
  
  print(sprintf("t = %.4f, df = %.4f, critical = %.4f and %.4f", t, df, crit, -1 * crit) )
  print(h1)
  print(paste("sample mean of x:", x ))
  print(sprintf("pval: %.4f, alpha = %.4f", pval, alpha ))
  if(alternative == "two.sided"){
    conf = c(x - crit * m, x + crit * m)
    print(sprintf("%.3f percent confidence interval: %.4f, %.4f", 1 - alpha, conf[1], conf[2]))
  }
  if(pval > alpha){
    print("fail to reject Ho")
  }else{
    print("reject Ho")
  }
}

v1 = c(39,32,41,46,27,33,31,32)

v2 = c(46,41,39,47,35,29,35,39)

ttest(mean(v2) - mean(v1), 0, 4.89 , length(v1), alternative = "two.sided")

df.calc <- function(sa, sb, na, nb){
  numerator <- (sa^2/na + sb^2/nb)^2
  denom <- (sa^2/na)^2/(na-1) + (sb^2/nb)^2/(nb-1)
  numerator/denom
}


# Suppose you are interviewing for a postition (assistant, babysitter, spouse, etc.), and candidates
# are interviewed one at a time with the following stipulations:
# 1. Candidates are ordered randomly
# 2. You could rank all the candidates if you were able to see the entire group
# 3. Candidates are independent of one another
# 4. Once you reject a candidate, you cannot recall them

# Given these guidelines, it turns out that the best strategy is to reject the first n/e candidates
# and then pick the next candidate that is better than the best rejected one. Doing so will nab
# you the best candidate with probability 1/e for a large number of candidates (~36.8% of the time).

n <- 100 #number of candidates
trials <- 10000
count <- 0
for(i in 1:trials){
  candidates <- rank(rnorm(n)) # Here the 'best' will have rank = n, i.e. rank 1 is the worst
  best <- max(candidates)
  
  reject <- candidates[1:round(length(candidates)/exp(1))]
  reject.max <- max(reject)
  
  new.candidates <- tail(candidates, length(candidates) - round(length(candidates)/exp(1)))
  our.choice <- new.candidates[new.candidates > reject.max][1]
  if(is.na(our.choice) == FALSE && our.choice == best){count <- count + 1}
}
count/trials


# What is interesting about this result is that it's equally likely that we do not pick
# any candidate at all: Since we reject the first n/e, and since all arrangements are 
# equally likely, there is an 1/e probability that the best candidate is in those first
# n/e rejected. 

# Furthermore, we have a little more than a 60% chance of selecting a candidate that is 
# among the top .5% of the best candidates. Not too shabby, I say:

#################################

n <- 100 #number of candidates
trials <- 10000
top <- 0 #
for(i in 1:trials){
  candidates <- rank(rnorm(n)) # Here the 'best' will have rank = n, i.e. rank 1 is the worst
  best <- max(candidates)
  
  reject <- candidates[1:round(length(candidates)/exp(1))]
  reject.max <- max(reject)
  
  new.candidates <- tail(candidates, length(candidates) - round(length(candidates)/exp(1)))
  our.choice <- new.candidates[new.candidates > reject.max][1]
  top.5 <- tail(sort(candidates), round(n * 0.05)) 
  if(is.na(our.choice) == FALSE && our.choice %in% top.5){top <- top + 1}
}
top/trials
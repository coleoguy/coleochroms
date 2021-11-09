probs <- c()
for(i in 1:100){
  print(i)
  foo <- histories[[i]]
  results <- describe.simmap(foo)
  probs[i] <- results$ace[1,1]
}
sum(probs)

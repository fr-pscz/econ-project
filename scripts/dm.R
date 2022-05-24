library(devtools)
library(dirichletprocess)
library(parallel)
library(coda)
library(dplyr)
library(tidyr)
library(numbers)

mdobj <- GaussianMixtureCreate()
dphmm1 <- DirichletHMMCreate(df$logHENRY, mdobj, 
                             alpha = 1, beta = 1)
dphmm2 <- DirichletHMMCreate(df$logHENRY, mdobj, 
                             alpha = 0.1, beta = 0.1)

its <- 7500
dpList <- list(dphmm1, dphmm2)

dpList <- mclapply(dpList, Fit, its=its, progressBar=T, mc.cores=3)

paramCoda <- mcmc.list(lapply(dpList, 
                              function(x) mcmc(cbind(Alpha = x$alphaChain, 
                                                     Beta = x$betaChain))))

averageStateAllocation1 <- rowMeans(data.frame(dpList[[1]]$statesChain[-(1:its/2)]))
averageStateAllocation2 <- rowMeans(data.frame(dpList[[2]]$statesChain[-(1:its/2)]))

avgStates <- rbind(
  data.frame(Time = df$date, StateAllocation = averageStateAllocation1, Params = "1"),
  data.frame(Time = df$date, StateAllocation = averageStateAllocation2, Params = "0.1")
)

avgStates %>% gather(Model, StateAllocation, -Params) -> avgStatesTidy

ggplot(avgStates, aes(x=Time, y=StateAllocation, colour=Params)) + 
  geom_point() + 
  theme(legend.position = "bottom") +
  xlab("") + 
  ylab("Average State Allocation")

stateAlloc <- data.frame(dpList[[1]]$statesChain[-(1:its/2)])
avg_alloc <- rowMeans(stateAlloc)
lq_alloc <- apply(stateAlloc, 1, quantile, probs = 0.05)
uq_alloc <- apply(stateAlloc, 1, quantile, probs = 0.95)
allocFrame <- data.frame(Time = df$date, AvgAlloc = avg_alloc, LQ_Alloc=  lq_alloc, UQ_Alloc = uq_alloc)

ggplot(allocFrame, aes(x=Time, y=AvgAlloc, ymin=LQ_Alloc, ymax=UQ_Alloc)) + 
  geom_line() + 
  geom_ribbon(alpha=0.25) + 
  xlab("") + 
  ylab("Average State Allocation")

# ====

burnin <- (its/2):its
allMus <- vector("list", length(burnin))
allSigmas <- vector("list", length(burnin))

for(i in seq_along(burnin)){
  allparams <- dpList[[1]]$paramChain[[burnin[i]]][dpList[[1]]$statesChain[[burnin[i]]]]
  allMus[[i]] <- sapply(allparams, "[[", 1)
  allSigmas[[i]] <- sapply(allparams, "[[", 2)
}
allMusMatrix <- do.call(cbind, allMus)
allSigmasMatrix <- do.call(cbind, allSigmas)

avgMus <- rowMeans(allMusMatrix)
avgSigmas <- rowMeans(allSigmasMatrix)

lqMus <- apply(allMusMatrix, 1, quantile, probs=0.05)
uqMus <- apply(allMusMatrix, 1, quantile, probs=0.95)

lqSigmas <- apply(allSigmasMatrix, 1, quantile, probs=0.05)
uqSigmas <- apply(allSigmasMatrix, 1, quantile, probs=0.95)

df %>% 
  mutate(AvgState = averageStateAllocation1, 
         Mu = avgMus, 
         LQ_Mu = lqMus,
         UQ_Mu = uqMus,
         Sigma = avgSigmas,
         LQ_Sigma = lqSigmas, 
         UQ_Sigma = uqSigmas,
         DiffState = c(NA, diff(AvgState)),
         StateChange = if_else(DiffState > 0.05, 1, 0)) -> trainFrame

trainFrame %>% filter(StateChange != 0) -> stateChangesDF
trainFrame %>% mutate(PlotColour = if_else(mod(round(AvgState),2) > 0, "Positive", "Negative")) -> trainFrame

ggplot(trainFrame, aes(x=date, y=logHENRY, colour=PlotColour)) + 
  geom_point(size = 0.5) + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  xlab("") + 
  ylab("Price")



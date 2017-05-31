library(progress)
totalIterationST = 0
totalIterationMTRS = 0
totalIterationERC = 0
totalIterationDSTARS = 0
totalIterationDSTARST = 0
totalIterationDRS = 0
if("ST" %in% mt.techs){ #Algorithm * Targets
  totalIterationST = length(techs)*sum(n.targets)*folds.num
}
if("MTRS" %in% mt.techs){ #Algorithms * Targets + Targets
  totalIterationMTRS = length(techs)*(sum(n.targets))+(sum(n.targets))*folds.num
}
if("ERC" %in% mt.techs){ #Algorithms * Targets * Ten permutations
  if(factorial(n.targets)<10)
    totalIterationERC = length(techs)*folds.num*sum(n.targets)+sum(n.targets)
  else
    totalIterationERC = length(techs)*10*folds.num*sum(n.targets)+sum(n.targets)
}
if("DRS" %in% mt.techs){
  totalIterationDRS = length(techs)*(sum(n.targets))*folds.num*sum(n.targets)*number.layers*n.folds.tracking
}
if("DSTARS" %in% mt.techs){
  totalIterationDSTARS = length(techs)*(sum(n.targets))*n.folds.tracking*folds.num
}
if("DSTARST" %in% mt.techs){
  totalIterationDSTARST = length(techs)*(sum(n.targets))*n.folds.tracking*folds.num*10
}

totalIteration = totalIterationST+totalIterationMTRS+totalIterationERC+totalIterationDSTARS+totalIterationDRS+totalIterationDSTARST

pb <- progress_bar$new(format = "  Executing for (:elapsed) [:bar] :percent  (:eta)",
                       total = totalIteration)

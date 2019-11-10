library(gridExtra)
library(dplyr)
library(ggplot2)
library(reshape2)

source("recombination.R")
source("mutation.R")
source("plotting.R")
source("generalFunctionality.R")
source("objectiveFunctions.R")

#' robotEA
#'
#' Main function for the evolutionary algorithm
#'
#' @param fun Objective Function. Accepts a robot and returns a single numeric as objective function value
#' @param budget Amount of allowed objective function evaluations
#' @param plotBestAfter Create a plot after this amount of algorithm iterations. 0 means only a final plot
#' @param popSize Population size of the EA
robotEA <- function(fun, budget, plotBestAfter = 0, popSize = 3){
    pop = list()
    for(i in 1:popSize){
        newRobot <- createRandomRobot()
        pop[[i]] <- list(robot = newRobot, fit = fun(newRobot))
        budget <- budget - 1
    }
    
    popOrder <- function(pop){
        getFit <- function(element){
            element$fit
        }
        fits <- sapply(pop, getFit)
        return(order(fits))
    }
    
    pop <- pop[popOrder(pop)]
    plotCounter <- 0
    while(budget > 0){
        plotCounter <- plotCounter + 1
        print(paste("Remaining Budget:", budget, "Best Fitness:", pop[[1]]$fit))
        
        ## Plot if criteria met
        ## 
        if(plotBestAfter != 0 && ((plotCounter %% plotBestAfter) == 0)){
            p <- plot(pop[[1]]$robot,fitness = pop[[1]]$fit)
            print(p)
            readline(prompt="Press [enter] to continue")
        }
        
        ## Mutation Add
        ## 
        parent <- pop[[sample(1:popSize,1,prob = popSize:1)]]$robot
        newRobot <- addChildRandom(parent)
        if(!is.null(newRobot)){
            pop[[length(pop)+1]] <- list(robot = newRobot, fit = fun(newRobot))
            budget <- budget -1
        }
        
        ## Mutation Remove
        ## 
        parent <- pop[[sample(1:popSize,1,prob = popSize:1)]]$robot
        newRobot <- removeChildRandom(parent)
        if(!is.null(newRobot)){
            pop[[length(pop)+1]] <- list(robot = newRobot, fit = fun(newRobot))
            budget <- budget -1
        }
        
        ## Recombination
        ## 
        parents <- pop[sample(1:popSize,2,prob = popSize:1)]
        newRobot <- crossOver(parents[[1]]$robot, parents[[2]]$robot)
        if(!is.null(newRobot)){
            pop[[length(pop)+1]] <- list(robot = newRobot, fit = fun(newRobot))
            budget <- budget -1
        }
        
        ## Sort Pop
        ## 
        pop <- pop[popOrder(pop)]
        
        ## Selection
        ## 
        pop <- pop[1:popSize]
    }
    ## Final Plot
    ## 
    p <- plot(pop[[1]]$robot,fitness = pop[[1]]$fit)
    print(p)
}



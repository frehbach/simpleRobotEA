addChildAt <- function(robot, ind, count = 0){
    count <- count + 1
    if(count == ind){
        freeDocks <- getFreeDockPoint(robot)
        if(length(freeDocks) > 0){
            robot$children[[length(robot$children)+1]] <- createRandomPart(freeDocks)
        }
        return(list("robot" = robot,"count" = count))
    }else{
        if(length(robot$children)>0){
            for(i in 1:length(robot$children)){
                addRes <- addChildAt(robot$children[[i]],ind,count)
                robot$children[[i]] <- addRes$robot
                count <- addRes$count
            }
        }
    }
    return(list("robot" = robot,"count" = count))
}

addChildRandom <- function(robot){
    partCount <- getPartCount(robot)
    chosenInd <- sample(1:partCount,1)
    
    newRobot <- addChildAt(robot,chosenInd)$robot
    
    if(isValid(newRobot)){
        return(newRobot)
    }else{
        print("Mutation generated invalid Robot")
        return(NULL)
    }
}

removeChildAt <- function(robot, ind, count = 0){
    count <- count + 1
    if(count == ind){
        return(list("robot" = "empty", "count" = count))
    }else{
        if(length(robot$children)>0){
            for(i in 1:length(robot$children)){
                res <- removeChildAt(robot$children[[i]],ind,count)
                robot$children[[i]] <- res$robot
                count <- res$count
            }
            for(i in length(robot$children):1){
                if(!typeof(robot$children[[i]]) == "list"){
                    robot$children[[i]] <- NULL
                }
            }
        }
    }
    return(list("robot" = robot,"count" = count))
}

removeChildRandom <- function(robot){
    partCount <- getPartCount(robot)
    
    ## Cant Remove an element if only core exists
    if(partCount < 2){
        return(robot)
    }
    chosenInd <- sample(2:partCount,1)
    return(removeChildAt(robot,chosenInd)$robot)
}
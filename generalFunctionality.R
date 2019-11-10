createRandomPart <- function(freeDockPoints = POSSIBLE_DOCK_POINTS){
    part <- list(
        "type" = sample(POSSIBLE_TYPES,1),
        "orientation" = sample(POSSIBLE_ORIENTATIONS,1),
        "dockPoint" = sample(freeDockPoints,1),
        #####
        #####       1
        #####     2 X 4 
        #####       3
        #####
        "children" = list()
    )
    class(part) <- "RobotPart"
    return(part)
}

createRandomRobot <- function(nIters = 5){
    r <- createRandomPart()
    for(i in 1:nIters){
        newR <- addChildRandom(r)
        if(!is.null(newR)){
            r <- newR
        }
    }
    return(r)
}

getPartCount <- function(robot, count = 0){
    count <- count + 1
    for(child in robot$children){
        count <- count + getPartCount(child)
    }
    return(count)
}

getFreeDockPoint <- function(robot){
    freeDocks <- 1:4
    
    ## Direction of Parent is already taken
    if(robot$dockPoint == 1) freeDocks <- freeDocks[-3]
    if(robot$dockPoint == 2) freeDocks <- freeDocks[-4]
    if(robot$dockPoint == 3) freeDocks <- freeDocks[-1]
    if(robot$dockPoint == 4) freeDocks <- freeDocks[-2]
    
    for(child in robot$children){
        freeDocks <- freeDocks[-which(freeDocks == child$dockPoint)]
    }
    return(freeDocks)
}

isValid <- function(robot){
    df <- robotToDF(robot)
    uniqueDf <- dplyr::distinct(df[,1:2])
    if(nrow(df) == nrow(uniqueDf)){
        return(T)
    }
    return(F)
}

robotToDF <- function(robot){
    m <- robotToMatrix(robot)
    return(data.frame(x = m[,1], y = m[,2], type = m[,3], dockPoint = m[,4]))
}

robotToMatrix <- function(robot){
    r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
    for(child in robot$children){
        childDF <- robotToMatrix(child)
        if(child$dockPoint == 1){
            childDF[,2] <- childDF[,2] + 1
        }
        if(child$dockPoint == 2){
            childDF[,1] <- childDF[,1] - 1
        }
        if(child$dockPoint == 3){
            childDF[,2] <- childDF[,2] - 1
        }
        if(child$dockPoint == 4){
            childDF[,1] <- childDF[,1] + 1
        }
        r <- rbind(r,childDF)
    }
    r
}

getRobotFromID <- function(robot, ind, count = 0){
    count <- count + 1
    if(count == ind){
        return(list("robot" = robot,"count" = count))
    }else{
        if(count > ind){
            return(list("robot" = "","count" = count))
        }
        if(length(robot$children)>0){
            for(i in 1:length(robot$children)){
                res <- getRobotFromID(robot$children[[i]],ind,count)
                count <- res$count
                if(typeof(res$robot) == "list"){
                    return(list("robot" = res$robot,"count" = count))
                }
            }
            return(list("robot" = "","count" = count))
        }else{
            return(list("robot" = "","count" = count))
        }
    }
    stop("Robot ID not found!")
}

robotToYaml <- function(robot){
    ###
    ###
    ###
    ###
}
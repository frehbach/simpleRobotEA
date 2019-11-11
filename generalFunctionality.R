createRandomPart <- function(freeDockPoints = POSSIBLE_DOCK_POINTS){
    type <- sample(POSSIBLE_TYPES,1)
    part <- list(
        "type" = type,
        "orientation" = ifelse(type == 2,sample(POSSIBLE_ORIENTATIONS,1),0),
        "dockPoint" = ifelse(type == 2,2,sample(freeDockPoints,1)),
        #####
        #####       1
        #####     3 X 2 
        #####       0
        #####
        "children" = list()
    )
    class(part) <- "RobotPart"
    return(part)
}

createCore <- function(){
    part <- list(
        "type" = 1, ## Core Type is 1
        "orientation" = 0, ## Core Orientation is 0
        "dockPoint" = 0, ## Core is not docked anywhere
        "children" = list()
    )
    class(part) <- "RobotPart"
    return(part)
}

createRandomRobot <- function(nIters = 5){
    r <- createCore()
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
    if(robot$type == 2){
        freeDocks <- 2
    }else{
        freeDocks <- 2:4
    }
    
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

robotToMatrix_old <- function(robot){
    if(robot$type == 1){
        r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
        for(i in -1:1){
            for(j in -1:1){
                if(i==0 && j==0){
                    next
                }
                r <- rbind(r, matrix(c(i,j,robot$type,robot$dockPoint), nrow = 1))
            }
        }
    }else{
        r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
    }
    for(child in robot$children){
        childDF <- robotToMatrix(child)
        if(robot$type == 1){
            moveFactor <- 2
        }else{
            moveFactor <- 1
        }
        if(child$dockPoint == 1){
            childDF[,2] <- childDF[,2] + moveFactor
        }
        if(child$dockPoint == 2){
            childDF[,1] <- childDF[,1] - moveFactor
        }
        if(child$dockPoint == 3){
            childDF[,2] <- childDF[,2] - moveFactor
        }
        if(child$dockPoint == 4){
            childDF[,1] <- childDF[,1] + moveFactor
        }
        r <- rbind(r,childDF)
    }
    r
}

robotToMatrixNonRecursive <- function(robot){
    ## putCore
    r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
    for(i in -1:1){
        for(j in -1:1){
            if(i==0 && j==0){
                next
            }
            r <- rbind(r, matrix(c(i,j,robot$type,robot$dockPoint), nrow = 1))
        }
    }
    
    listOfChildren <- robot$children
    while(length(listOfChildren) > 0){
        newListOfChildren <- list()
        for(child in listOfChildren){
            r <- rbind(r, matrix(c(i,j,child$type,child$dockPoint), nrow = 1))
        }
    }
}

robotToMatrix <- function(robot){
    if(robot$type == 1){
        r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
        for(i in -1:1){
            for(j in -1:1){
                if(i==0 && j==0){
                    next
                }
                r <- rbind(r, matrix(c(i,j,robot$type,robot$dockPoint), nrow = 1))
            }
        }
    }else{
        r <- matrix(c(0,0,robot$type,robot$dockPoint), nrow = 1)
    }
    for(child in robot$children){
        childDF <- robotToMatrix(child)
        
        ## rotate childDF
        ##
        if(child$dockPoint == 3){
            newX <- -childDF[,2]
            childDF[,2] <- childDF[,1]
            childDF[,1] <- newX
        }else if(child$dockPoint == 1){
            childDF[,2] <- -childDF[,2]
            childDF[,1] <- -childDF[,1]
        }else if(child$dockPoint == 4){
            newX <- childDF[,2]
            childDF[,2]<- -childDF[,1]
            childDF[,1] <- newX
        }
        
        ## move child
        ## 
        if(robot$type == 1){
            moveFactor <- 2
        }else{
            moveFactor <- 1
        }
        if(child$dockPoint == 1){
            childDF[,2] <- childDF[,2] - moveFactor
        }
        if(child$dockPoint == 2){
            childDF[,2] <- childDF[,2] + moveFactor
        }
        if(child$dockPoint == 3){
            childDF[,1] <- childDF[,1] + moveFactor
        }
        if(child$dockPoint == 4){
            childDF[,1] <- childDF[,1] - moveFactor
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

robotToYaml <- function(robot, file = "autoGeneratedRobot.yaml"){
    lines <- c("---","id: autoGeneratedRobot", "body:")
    lines <- c(lines, robotPartsToYaml(robot))
    lines <- c(lines, "brain:", "    type: rlpower-splines")
    writeLines(lines,con = file)
}

robotPartsToYaml <- function(x, tabIndex = 1, runningID = 0){
    runningID <- runningID + 1
    lines = NULL
    
    if(x$type != 1){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),x$dockPoint-1,":"))
        tabIndex <- tabIndex + 1
    }
    
    ## Paste ID to File
    if(x$type == 1){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"id: Core"))
    }else{
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"id: Element",runningID))
    }
    
    ## Paste Type to File
    if(x$type == 1){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"type: Core"))
    }else if(x$type == 2){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"type: ActiveHinge"))
    }else if(x$type == 3){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"type: FixedBrick"))
    }
    
    ## Paste Colors
    if(x$type == 1){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"params:"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"red: 0.94"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"green: 0.04"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"blue: 0.05"))
    }else if(x$type == 2){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"params:"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"red: 0.98"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"green: 0.98"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"blue: 0.98"))
    }else if(x$type == 3){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"params:"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"red: 0.05"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"green: 0.26"))
        lines <- c(lines, paste0(paste(rep("    ",tabIndex + 1),collapse=""),"blue: 0.72"))
    }
    
    ## Paste Orientation
    lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"orientation: ",x$orientation))
    
    if(length(x$children)>0){
        lines <- c(lines, paste0(paste(rep("    ",tabIndex),collapse=""),"children:"))
        tabIndex <- tabIndex + 1
    }
    for(child in x$children){
        lines <- c(lines, robotPartsToYaml(child, tabIndex = tabIndex, runningID))
    }
    return(lines)
}

createAndPlotRandom <- function(nIters = 5){
    robot <- createRandomRobot(nIters)
    print(robot)
    print(plot(robot))
    return(robot)
}

source("ea.R")

POSSIBLE_TYPES <- 1:100
POSSIBLE_DOCK_POINTS <- 1:4
POSSIBLE_ORIENTATIONS <- c(0,90)

## Objective: Create a long slim robot
robotEA(objectiveLongRobot,100,10,3)

## Objective most pieces into limited space
robotEA(objectiveMaximumPiecesIntoLength10,1000,100,3)

## Same with more budget and without Stops:
robotEA(objectiveLongRobot,200,0,3)
robotEA(objectiveMaximumPiecesIntoLength10,1000,0,3)

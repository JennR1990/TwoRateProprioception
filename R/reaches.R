
# for now, we assume the RateRate package is installed and just load it:
#library(RateRate)

loadGroupReaches <- function(group) {
  
  # load the right file, easy because we have good file naming conventions:
  return(read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors=F))
  
}

getMedianGroupReaches <- function(group, FUN=median) {
  
  # load file:
  df <- loadGroupReaches(group)
  
  # calculations can be done in one line, and we still return a nice data frame:
  return(data.frame(schedule=df$distortion, reachdeviation_deg=apply(as.matrix(df[,2:dim(df)[2]]), c(1), FUN, na.rm=T)))
  
}


getGroupReachConfidenceIntervals <- function(group) {
  
  # load file:
  df <- loadGroupReaches(group)
  
  # unfinished function..............................................
  # return()
}

fitModelsToAllGroups <- function(groups=c('active','passive','nocursor','pause')) {
  
  for (group in groups) {
    
    RD <- getMedianGroupReaches(group=group)
    reaches <- RD$reachdeviation_deg
    schedule <- RD$schedule
    
    param <- fitTwoRateReachModel(reaches = reaches, schedule = schedule, grid='skewed', gridsteps=8)
    cat(sprintf('condition: %s\n', group))
    print(param)
  }
  
  
}
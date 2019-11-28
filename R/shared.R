getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}

Loaddata<- function (group='passive', task='reaches') {
  # filename <- sprintf('data/%s_%s.csv',group,task)
  # df <- read.csv(filename, stringsAsFactors=F)
  return(read.csv(sprintf('data/%s_%s.csv',group,task), stringsAsFactors=F))
}
 
loadalldata<- function () {
  pause_reaches<<- Loaddata(group='pause')
  Pause<<- Loaddata(group='pause', task = 'pre_post_Prop')
  active_reaches<<- Loaddata(group='active')
  passive_reaches<<- Loaddata()
  nocursor_reaches<<- Loaddata(group='nocursor')
  nocursorI_reaches<<- Loaddata(group='nocursor', task = 'NI_reaches')
  passive_localization<<- Loaddata(task = 'localization')
  active_localization<<- Loaddata(group='active', task = 'localization')
  nocursor_nocursors<<- Loaddata(group='nocursor', task = 'nocursors')
  NoCursor<<- Loaddata(group='NoCursor', task = 'pre_post_Prop')
  nocursorI_nocursors<<- Loaddata(group='nocursor', task = 'NI_nocursors')
  NewNoC<<- Loaddata(group='NewNoC', task = 'pre_post_Prop') 
}

downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('active_localization.csv'  = 'https://osf.io/mc523/?action=download',
             'active_reaches.csv'       = 'https://osf.io/ejxy9/download',
             'nocursor_nocursors.csv'   = 'https://osf.io/5b8s9/download',
             'nocursor_reaches.csv'     = 'https://osf.io/vmnx7/download',
             'nocursor_NI_nocursors.csv'   = 'https://osf.io/y4k2x/download',
             'nocursor_NI_reaches.csv'     = 'https://osf.io/grnxh/download',
             'passive_localization.csv' = 'https://osf.io/27v54/download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/download',
             'pause_reaches.csv'        = 'https://osf.io/q59b3/download',
             'NewNoC_pre_post_Prop.csv' = 'https://osf.io/r69v5/download',
             'NoCursor_pre_post_Prop.csv' = 'https://osf.io/tjqrw/download',
             'Pause_pre_post_Prop.csv'  = 'https://osf.io/r3fvw/download')

  # check if data directory exists and create if necessary:
  # (data should come from OSF, so is not on github)
  if (!dir.exists('data')) {
    dir.create('data')
  }
  
  # check if each file exists and copy it if necessary: 
  for (filename in names(files)) {
    
    filepath <- sprintf('data/%s',filename)
    
    if (!file.exists(filepath) | update) {
      
      df <- read.csv(url(files[filename]), stringsAsFactors=F)
      write.csv(df, filepath, quote=FALSE, row.names=FALSE)
      
    }
    
  }
  
}


percentNAs <- function (df) {
  return((sum(is.na(df))/prod(dim(df)))*100)
}
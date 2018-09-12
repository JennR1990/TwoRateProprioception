
downloadOSFdata <- function(update=FALSE) {
  
  files <- c('active_localization.csv'  = 'https://osf.io/vys7e/?action=download',
             'active_reaches.csv'       = 'https://osf.io/nhcmb/?action=download',
             'nocursor_nocursors.csv'   = 'https://osf.io/b9fc8/?action=download',
             'nocursor_reaches.csv'     = 'https://osf.io/7x6b8/?action=download',
             'passive_localization.csv' = 'https://osf.io/27v54/?action=download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/?action=download',
             'pause_reaches.csv'        = 'https://osf.io/q59b3/?action=download')
  
  for (filename in names(files)) {
    
    filepath <- sprintf('data/%s',filename)
    
    if (!file.exists(filepath) | update) {
      
      df <- read.csv(url(files[filename]), stringsAsFactors=F)
      write.csv(df, filepath, quote=FALSE, row.names=FALSE)
      
    }
    
  }
  
}
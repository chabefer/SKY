#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 
setwd("C:/Dropbox/private/Private/SKY/SKY_R/")

#render your sweet site. 
rmarkdown::render_site()

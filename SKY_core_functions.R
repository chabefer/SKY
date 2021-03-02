## ---- Meta.Correct ----
MDE.var <- function(alpha=0.05,kappa=0.33,varE=1){
  return((qnorm(kappa)+qnorm(1-alpha/2))*sqrt(varE))
}

ppCurvePower <- function(p1,p2,alpha=0.05,kappa=0.33,varE=1){
  return((pnorm(MDE.var(alpha=alpha,kappa=kappa)-qnorm(1-p2/2))
          -pnorm(MDE.var(alpha=alpha,kappa=kappa)-qnorm(1-p1/2)))/kappa)
}

pCurve.hist <- function(pvalues,power=.33){
  pvaluessig <- pvalues[pvalues<=0.05]  
  dens1 <- sum(ifelse(abs(pvaluessig-0.005)<0.005,1,0)/length(pvaluessig))
  dens2 <- sum(ifelse(abs(pvaluessig-0.015)<0.005,1,0)/length(pvaluessig))
  dens3 <- sum(ifelse(abs(pvaluessig-0.025)<0.005,1,0)/length(pvaluessig))
  dens4 <- sum(ifelse(abs(pvaluessig-0.035)<0.005,1,0)/length(pvaluessig))
  dens5 <- sum(ifelse(abs(pvaluessig-0.045)<0.005,1,0)/length(pvaluessig))
  dens <- c(dens1,dens2,dens3,dens4,dens5)
  p.hist.1 <- cbind(c(0.01,0.02,0.03,0.04,0.05),dens)
  p.hist.1 <- as.data.frame(p.hist.1)
  colnames(p.hist.1) <- c('p','density')
  p.hist.1$Data <- c("Observed")
  p.hist.2 <- cbind(c(0.01,0.02,0.03,0.04,0.05),0.2)
  p.hist.2 <- as.data.frame(p.hist.2)
  colnames(p.hist.2) <- c('p','density')
  p.hist.2$Data <- c("Uniform")
  dens331 <- ppCurvePower(0,0.01)
  dens332 <- ppCurvePower(0.01,0.02)
  dens333 <- ppCurvePower(0.02,0.03)
  dens334 <- ppCurvePower(0.03,0.04)
  dens335 <- ppCurvePower(0.04,0.05)
  dens33 <- c(dens331,dens332,dens333,dens334,dens335)
  p.hist.3 <- cbind(c(0.01,0.02,0.03,0.04,0.05),dens33)
  p.hist.3 <- as.data.frame(p.hist.3)
  colnames(p.hist.3) <- c('p','density')
  p.hist.3$Data <- c("Power33")
  p.hist <- rbind(p.hist.1,p.hist.2,p.hist.3)
  return(p.hist)
}


Meta.Correct <- function(table,SQLcon){
  data <- dbReadTable(SQLcon,table)
  #--------------#
  # FAT-PET-PEESE
  #--------------#
  # weights for WLS estimator
  data <- data %>% 
    mutate(weightsWLS=(1/v)/(sum(1/v)))
  # WLS Meta estimator
  data.WLS.Meta <- lm(d~1,weights = weightsWLS,data=data)
  # WLS FAT PET estimator
  data.WLS.FAT.PET <- lm(d~sed,weights = weightsWLS,data=data)
  # WLS PEESE estimator
  data.WLS.PEESE <- lm(d~v,weights = weightsWLS,data=data)
  # FE Meta Estimator
  data.FE.Meta <- rma(yi=d,vi=v,data=data,method="FE")
  # FE FAT PET estimator
  data.FE.FAT.PET <- rma(d~sed,vi=v,data=data,method="FE")
  # FE PEESE estimator
  data.FE.PEESE <- rma(d~v,vi=v,data=data,method="FE")
  # RE Meta Estimator
  data.RE.Meta <- rma(yi=d,vi=v,data=data,method="REML")
  # RE FAT PET estimator
  data.RE.FAT.PET <- rma(d~sed,vi=v,data=data,method="REML")
  # RE PEESE estimator
  data.RE.PEESE <- rma(d~v,vi=v,data=data,method="REML")
  
  # Results #
  #---------#
  MetaWLS <- coef(data.WLS.Meta)[[1]]
  MetaWLS.se <- sqrt(diag(vcov(data.WLS.Meta)))[[1]]
  FAT.slope <- coef(data.WLS.FAT.PET)[[2]]
  FAT.slope.se <- sqrt(diag(vcov(data.WLS.FAT.PET)))[[2]]
  FAT.pvalue <- 1-pnorm(FAT.slope/FAT.slope.se)
  PET <- coef(data.WLS.FAT.PET)[[1]]
  PET.se <- sqrt(diag(vcov(data.WLS.FAT.PET)))[[1]]
  PET.pvalue <- 2*(1-pnorm(PET/PET.se))
  PEESE <-  coef(data.WLS.PEESE)[[1]]
  PEESE.se <- sqrt(diag(vcov(data.WLS.PEESE)))[[1]]
  fatpetpeese_s <- ifelse(FAT.pvalue>0.05,MetaWLS,
                          ifelse(PET.pvalue>0.05,PET,PEESE))
  sef <- ifelse(FAT.pvalue>0.05,MetaWLS.se,
                          ifelse(PET.pvalue>0.05,PET.se,PEESE.se))
  
  # plot of results
  colors <- c("MetaAnalysisOriginal"="red","Replication"="blue","MetaAnalysisWLS"="green","MetaAnalysisRE"="purple")
  lines <- c("FATPETWLS"="solid","PEESEWLS"="dashed","FATPETRE"="dotted","PEESERE"="dotdash")
  FATPETPEESEPlot <- ggplot(data,aes(x=sed,y=d))+
                      geom_point()+
                      xlim(0,0.5)+
                      ylim(-0.1,1)+
                      theme_bw()+
                      xlab("Standard error")+
                      ylab("Effect size")+
                      geom_hline(aes(yintercept=Aggregate %>%
                                       filter(str_detect(metaanalysis,"data")==TRUE) %>%
                                       select(meta_s) %>% 
                                       as.numeric(),colour="MetaAnalysisOriginal"))+
                      geom_hline(aes(yintercept=Aggregate %>%
                                       filter(str_detect(metaanalysis,"data")==TRUE) %>%
                                       select(replication_s) %>% 
                                       as.numeric(),colour="Replication")) +
                      geom_hline(aes(yintercept=coef(data.WLS.Meta)[[1]],colour="MetaAnalysisWLS"))+
                      geom_hline(aes(yintercept=coef(data.RE.Meta)[[1]],colour="MetaAnalysisRE"))+
                      stat_function(fun=function(x){coef(data.WLS.FAT.PET)[1] + coef(data.WLS.FAT.PET)[2]*x},aes(linetype="FATPETWLS"),color="black")+
                      stat_function(fun=function(x){coef(data.WLS.PEESE)[1] + coef(data.WLS.PEESE)[2]*x^2},aes(linetype="PEESEWLS"),color="black")+
                      stat_function(fun=function(x){coef(data.RE.FAT.PET)[1] + coef(data.RE.FAT.PET)[2]*x},aes(linetype="FATPETRE"),color="black")+
                      stat_function(fun=function(x){coef(data.RE.PEESE)[1] + coef(data.RE.PEESE)[2]*x^2},aes(linetype="PEESERE"),color="black")+
                      scale_colour_manual("Estimate",values = colors)+
                      scale_linetype_manual("Method",values = lines)
  
  #----------#
  # Pcurving #
  #----------#
  data <- data %>%
    mutate(
      tstat = d/sed,
      pvalue= 2*(1-pnorm(tstat)))
  
  p.hist <- pCurve.hist(data$pvalue)
  p.hist$Data <- factor(p.hist$Data,levels=c("Observed","Uniform","Power33"))
  
  ggplot(data=p.hist, aes(x=p, y=density, color=Data)) +
    geom_point() +
    geom_line() +
    theme_bw()
  
}




## ---- Results.to.SQL ---- 
dbUpdateResults <- function(con,table,data,where=NA,update=NA,append=FALSE,overwrite=FALSE){
  table.exist <- dbGetQuery(con,paste("SHOW TABLES LIKE",paste("'",table,"'",sep=''),sep=' '))
  table.exist <- nrow(table.exist)
  if (overwrite==TRUE | table.exist==0){
    dbWriteTable(con, table,data,overwrite=TRUE,row.names=FALSE)
    # add primary key
    dbSendQuery(con,paste("ALTER TABLE",table,"ADD COLUMN result_id INT AUTO_INCREMENT PRIMARY KEY;",sep=' '))
    dbSendQuery(con,paste("ALTER TABLE",table,"MODIFY COLUMN result_id INT FIRST;",sep=' '))
  }
  if (append==FALSE & overwrite==FALSE & table.exist>0){
    results.non.exists <- c()
    for (i in 1:nrow(data)){
      SQL.message <- paste("SELECT","EXISTS","(","SELECT","*","FROM",table,"WHERE",paste(paste(paste("`",where,"`",sep=''),paste("'",sapply(data[i,where],as.character),"'",sep=''),sep='='),collapse=' AND '),")",";",sep=' ')
      exists <- dbGetQuery(con,SQL.message)
      if (exists==1){
        SQL.message <- paste('UPDATE',table,'SET',paste(paste(paste("`",update,"`",sep=''),data[i,update],sep='='),collapse=','), 'WHERE',paste(paste(paste("`",where,"`",sep=''),paste("'",sapply(data[i,where],as.character),"'",sep=''),sep='='),collapse=' AND '),";",sep=' ')
        SQL.message <- gsub('NA','NULL',SQL.message) 
        SQL.message <- gsub('NaN','NULL',SQL.message) 
        dbSendQuery(con,SQL.message)
      }else{
        results.non.exists <- c(results.non.exists,i)
      }
    }
    if (length(results.non.exists)>0){
      data <- data[results.non.exists,]
      append <- TRUE
    }
  }
  if (append==TRUE){
    # find primary key to increment
    message.SQL <- paste("SELECT `COLUMN_NAME` FROM `information_schema`.`COLUMNS` WHERE (`TABLE_NAME` =",paste("'",table,"'",sep=''),")","AND (`COLUMN_KEY` = 'PRI');",sep=' ')
    primary.key <- dbGetQuery(con,message.SQL)
    maxId <- dbGetQuery(con, paste("SELECT MAX(",primary.key,") FROM ",table,sep=''))[1,1] 
    maxId <- ifelse(is.na(maxId),0,maxId)
    data <- as.data.frame(cbind(maxId + 1:nrow(data),data))
    colnames(data)[1] <- c(as.character(primary.key))
    where <- c(as.character(primary.key),where)
    for (i in 1:nrow(data)){
      SQL.message <- paste('INSERT INTO',table,'SET',paste(c(paste(paste("`",where,"`",sep=''),paste("'",sapply(data[i,where],as.character),"'",sep=''),sep='='),paste(paste("`",update,"`",sep=''),data[i,update],sep='=')),collapse=','),";",sep=' ')
      SQL.message <- gsub('NA','NULL',SQL.message) 
      SQL.message <- gsub('NaN','NULL',SQL.message) 
      dbSendQuery(con,SQL.message)
    }  
  }
}

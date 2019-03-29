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

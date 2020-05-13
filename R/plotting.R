plottimeserie <- function(mytimeserie, graphtype, type, where,wherestate= "", filename=""){
  
  xaxistitle = "Date"
  if(wherestate!=""){
    wherestate = paste(sep = "","(",wherestate,")")
  }
  if(type=="Confirmeddiff"){
    yaxistitle = "Number of Confirmed"
    maintitle = paste("Number of Confirmed per day","in",where,wherestate)   
  }else if(type== "Deathsdiff"){
    yaxistitle = "Number of Death"
    maintitle = paste("Number of Death per day","in",where,wherestate) 
  } else if(type=="Recovereddiff"){
    yaxistitle = "Number of Recovered"
    maintitle = paste("Number of Recovered per day","in",where,wherestate) 
  } else if(type=="Activediff"){
    yaxistitle = "Number of Active"
    maintitle = paste("Number of Active per day","in",where,wherestate) 
  }else{
    yaxistitle = paste("Number of", type)
    maintitle = paste("Number of", type,"in",where,wherestate) 
    
  }
   
  if (graphtype == "Prediction"){
    fit <- auto.arima(mytimeserie)
    datelist = index(mytimeserie)
    datelist = format(datelist, "%b %d") 
    #datelist = format(datelist, "%b %d %y") 
    
    myfit =forecast(fit, h=90)
    jump = floor(length(datelist) / 3)
    #mydatelist <- structure(integer(), class = "Date")
    #for (adate in 1:length(datelist)) {
    # c(mydatelist,adate)
    #}
    
    xaxis = datelist[seq(1, length(datelist), by = jump)]
    xaxisbreaks = seq(1, length(datelist), by = jump)
    #xaxis <- ifelse(format(datelist, "%d")==1,datelist,NA)
    #df$state <- ifelse(df$date2<=as.Date("2007-12-16"),"calib","valid")
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    
     if (filename!=""){
       print("file")
       print(filename)
       p <- autoplot(forecast(fit)) + ggtitle(maintitle) +
         scale_x_continuous(name = xaxistitle,breaks = xaxisbreaks,labels = xaxis) + 
         scale_y_continuous(name = yaxistitle,labels = scales::comma) + 
         theme_solarized()+
         scale_colour_solarized()
      png(filename) 
      
      print(p)
      
      dev.off()
      if(ftp){
        savefiletoftp(filename)
      }
      #ggsave(filename,device = jpeg, width = 5, height = 5, units = "in")
      
       
     
     }else{
      print("no file")
        print(autoplot(forecast(fit)) + ggtitle(maintitle) +
         scale_x_continuous(name = xaxistitle,breaks = xaxisbreaks,labels = xaxis) + 
         scale_y_continuous(name = yaxistitle,labels = scales::comma) + 
         theme_solarized()+
         scale_colour_solarized())
       
       #dev.off() 
       
     }
  } else if (graphtype == "Real"){
   
    if (filename != ""){
      p <- autoplot(mytimeserie)+ ggtitle(maintitle)  + 
        scale_x_date(name = xaxistitle) + 
        scale_y_continuous(name = yaxistitle,labels = scales::comma) +
        theme_solarized() + scale_colour_solarized()
      png(filename) 
      print(p)
      dev.off() 
      }else{
        print("no file")
        print(autoplot(mytimeserie)+ ggtitle(maintitle)  + 
        scale_x_date(name = xaxistitle) + 
        scale_y_continuous(name = yaxistitle,labels = scales::comma) +
        theme_solarized() + scale_colour_solarized())
       

    }
  } else if(graphtype == "Multiple"){
    
  } else {
    print("Graph type must be Prediction or Real")  
  }
}

plotcoviddata <- function(dataframe, where, scope = "World",type = "Confirmed", graphtype = "Prediction", myfilename = "")
{
  print(paste(where,scope,type,graphtype,myfilename)) 
  if(type=="Confirmeddiff" || type=="Deathdiff" || type=="Recovereddiff" || type=="Activediff"){
    if(!("Confirmeddiff" %in% colnames(dataframe)))
    {
      print("Run the getdifferencialvalues on your dataframe before trying to plot periodic values")
    }
  }
  
  if(scope == "World"){
    filteredf <- dataframe
    if(nrow(filteredf) <= 2){
      print("Only two row or less. Can't draw")
      return()
    }
    dt <- data.table(filteredf)
    dt2 <- dt[,list(mytype = sum(eval(as.symbol(type))), freq = .N), by = c("date")]
    names(dt2)[names(dt2) == 'mytype'] <- type
    filteredf <- na.omit(setDF(dt2))
    df_ts <- xts(x = filteredf[type], order.by = filteredf$date )
    if (nrow(df_ts)<2){
      print("Timeserie too short")
      
      return()
    }
    where = " the World"
    plottimeserie(df_ts, graphtype,type,where,filename = myfilename)
  } else if(scope == "Country"){
    filteredf <- dataframe %>% filter(Country.Region == where)
    if(!isrecoveredpresent(filteredf) && (type == "Recovered" || type == "Recovereddiff")){
      #afilename = paste(sep="",myfilename,type,".png")
      #createblankfile(afilename)
      return()
    }
   
    #print(dt[])eval(as.symbol(type))
    if(nrow(filteredf) <= 2){
      print("Only two row or less. Can't draw")
      return()
    }
    dt <- data.table(filteredf)
    dt2 <- dt[,list(mytype = sum(eval(as.symbol(type))), freq = .N), by = c("date")]
    
    names(dt2)[names(dt2) == 'mytype'] <- type
    filteredf <- na.omit(setDF(dt2))
    df_ts <- xts(x = filteredf[type], order.by = filteredf$date )
    if (nrow(df_ts)<2){
      print("Timeserie too short")
      
      return()
    }
    afilename = paste(sep="",myfilename,type,".png")
    if(myfilename==""){
      afilename=""
    }
    
    plottimeserie(df_ts, graphtype,type,where,filename = afilename)
  } else if(scope == "State") {
    filteredf <- dataframe %>% filter(Province.State == where)
    if(!isrecoveredpresent(filteredf) && (type == "Recovered" || type == "Recovereddiff")){
      #afilename = paste(sep="",myfilename,type,".png")
      #createblankfile(afilename)
      return()
    }
    if(nrow(filteredf) <= 2){
      print("Only two row or less. Can't draw")
      return()
    }
    dt <- data.table(filteredf)
    dt2 <- dt[,list(mytype = sum(eval(as.symbol(type))), freq = .N), by = c("date")]
    names(dt2)[names(dt2) == 'mytype'] <- type
    filteredf <- na.omit(setDF(dt2))
    df_ts <- xts(x = filteredf[type], order.by = filteredf$date )
    if (nrow(df_ts)<2){
      print("Timeserie too short")
      
      return()
    }
    print(df_ts)
    afilename = paste(sep="",myfilename,type,".png")
    if(myfilename==""){
      afilename=""
    }
    plottimeserie(df_ts, graphtype,type,where,filename = afilename)
    
  } else if(scope == "City") {
    if(type == "Recovered" || type == "Recovereddiff"){
      return()
    }
    allstates <- sqldf(paste(sep = "","SELECT DISTINCT [Province.State] FROM dataframe WHERE Admin2 ='",gsub("'", "''", where),"'"))
    for(state in allstates$Province.State ){
       print(state) 
       filteredf <- dataframe %>% filter(Admin2 == where)
        filteredf <- filteredf %>% filter(Province.State == state)
        
        if(nrow(filteredf) <= 2){
          print("Only two row or less. Can't draw")
          next
        }
        dt <- data.table(filteredf)
        dt2 <- dt[,list(mytype = sum(eval(as.symbol(type))), freq = .N), by = c("date")]
        names(dt2)[names(dt2) == 'mytype'] <- type
        filteredf <- na.omit(setDF(dt2))
        df_ts <- xts(x = filteredf[type], order.by = filteredf$date )
        if (nrow(df_ts)<2){
          print("Timeserie too short")
          
          return()
        }
        afilename = paste(sep="",myfilename,"state",adaptfilename(state),type,".png")
        if(myfilename==""){
          afilename=""
        }
       
        plottimeserie(df_ts, graphtype,type,where,wherestate = state ,filename = afilename)
      } 
  }
  
  else{
    print("The scope must be : World, Country, State or City")  
  }
}

isrecoveredpresent <- function(mydataframe){
  testdf <- mydataframe %>% filter(Recovered != 0)
  if(nrow(testdf) == 0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
createblankfile <- function(filename){
  file.create(filename)  
  
}

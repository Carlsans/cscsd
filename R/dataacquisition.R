
downloadlatestdata <- function(today = FALSE){
  start <- as.Date("22-01-20",format="%d-%m-%y")
  #start <- Sys.Date() - 2
  if (today){
    end <- Sys.Date() 
  }else{
    end <- Sys.Date() - 1
    
  }
  theDate <- start
  data = NULL
  previous = NULL
  while (theDate <= end)
  {
    
    #print(paste0("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_daily_reports/",format(theDate,"%m-%d-%Y"),".csv"))
    filepath <- paste0("http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",format(theDate,"%m-%d-%Y"),".csv")
    print(filepath)
    previous <- data
    data <- read.csv(filepath)
    
    data$date = theDate
    if("Province_State" %in% colnames(data)){
      names(data)[names(data) == 'Province_State'] <- 'Province.State'
      
    }
    if("Country_Region" %in% colnames(data)){
      names(data)[names(data) == 'Country_Region'] <- 'Country.Region'
      
    }
    if("Last_Update" %in% colnames(data)){
      names(data)[names(data) == 'Last_Update'] <- 'Last.Update'
      
    }
    if("Lat" %in% colnames(data)){
      names(data)[names(data) == 'Lat'] <- 'Latitude'
      
    }
    if("Long_" %in% colnames(data)){
      names(data)[names(data) == 'Long_'] <- 'Longitude'
      
    }
    #class(previous)
    #print(previous)
    if(!is.null(previous)){
      data <- dplyr::bind_rows(data, previous)
    }
    
    theDate <- theDate + 1                    
  }
  data$Country.Region <- gsub('\\*', '', data$Country.Region)
  data <- distinct(data)
  #data$Confirmed[is.na(data$Confirmed)] <- 0
  return(data)
  
}
getdifferencialvalues <- function(basedataframe){
  cleaneddataframe <- basedataframe
  cleaneddataframe$Province.State[cleaneddataframe$Province.State==""]<-NA
  cleaneddataframe$Admin2[cleaneddataframe$Admin2==""]<-NA
  listallcountry = listlocations(cleaneddataframe, locationtype = "Country",viewlocation = FALSE)
  listallprovincestate = listlocations(cleaneddataframe, locationtype = "State",viewlocation = FALSE)
  listallcity = listlocations(cleaneddataframe, locationtype = "City",viewlocation = FALSE)
  diffdataframe <- NULL
  n <- 0
  n2 <- 0
  for (country in listallcountry) {
    progress <- n/length(listallcountry)
    percentdone <- paste0(formatC(100 * progress, format = "f", digits = 2), "%")
    print(paste("Progress Country:",percentdone))
    n <- n + 1
    filteredbycountry <- cleaneddataframe %>% filter(Country.Region == country)
    # all(is.na(c(NA, NaN)))
    if(all(is.na(filteredbycountry$Province.State)) && all(is.na(filteredbycountry$Admin2))){
      # All those have no state or city specified
      sqlrequest = paste(sep = "", "SELECT a.date, a.[Country.Region], a.[Province.State], a.Admin2, a.Latitude, a.Longitude, a.Confirmed,a.Deaths, a.Recovered, a.Active"
                         ,"      , a.Confirmed - (SELECT b.Confirmed"
                         ,"                   FROM filteredbycountry b"
                         ,"                   WHERE b.date < a.date"
                         ,"                   AND b.[Country.Region] = '", gsub("'", "''", country), "'"
                         ," AND b.Admin2 IS NULL AND b.[Province.State] IS NULL"
                         ,"                   ORDER BY b.date DESC"
                         ,"                   ) Confirmeddiff"
                         
                         
                         ,"      , a.Deaths - (SELECT b.Deaths"
                         ,"                   FROM filteredbycountry b"
                         ,"                   WHERE b.date < a.date"
                         ,"                   AND b.[Country.Region] = '", gsub("'", "''", country), "'"
                         ," AND b.Admin2 IS NULL AND b.[Province.State] IS NULL"
                         ,"                   ORDER BY b.date DESC"
                         ,"                   ) Deathsdiff"
                         
                         ,"      , a.Recovered - (SELECT b.Recovered"
                         ,"                   FROM filteredbycountry b"
                         ,"                   WHERE b.date < a.date"
                         ,"                   AND b.[Country.Region] = '", gsub("'", "''", country), "'"
                         ," AND b.Admin2 IS NULL AND b.[Province.State] IS NULL"
                         ,"                   ORDER BY b.date DESC"
                         ,"                   ) Recovereddiff"
                         
                         ,"      , a.Active - (SELECT b.Active"
                         ,"                   FROM filteredbycountry b"
                         ,"                   WHERE b.date < a.date"
                         ,"                   AND b.[Country.Region] = '", gsub("'", "''", country), "'"
                         ," AND b.Admin2 IS NULL AND b.[Province.State] IS NULL"
                         ,"                   ORDER BY b.date DESC"
                         ,"                   ) Activediff"
                         
                         
                         ," FROM filteredbycountry a WHERE a.[Country.Region] = '", gsub("'", "''", country), "'"
                         ," AND a.Admin2 IS NULL AND a.[Province.State] IS NULL"
      )
      filterbycountrywithdiff =  sqldf(sqlrequest)
      diffdataframe <- rbind(diffdataframe, filterbycountrywithdiff)   
    } else if ( all(is.na(filteredbycountry$Admin2))){
      # All those have no city specified
      #print(country)
      
      for(provincestate in listallprovincestate){
        filterbyprovincestate <-  filteredbycountry %>% filter(Province.State == provincestate)
        sqlrequest = paste(sep = "", "SELECT a.date, a.[Country.Region], a.[Province.State], a.Admin2, a.Latitude, a.Longitude, a.Confirmed,a.Deaths, a.Recovered, a.Active"
                           ,"      , a.Confirmed - (SELECT b.Confirmed"
                           ,"                   FROM filterbyprovincestate b"
                           ,"                   WHERE b.date < a.date"
                           ,"                   AND b.[Province.State] = '", gsub("'", "''", provincestate), "'"
                           ," AND b.Admin2 IS NULL"
                           ,"                   ORDER BY b.date DESC"
                           ,"                   ) Confirmeddiff"
                           
                           
                           ,"      , a.Deaths - (SELECT b.Deaths"
                           ,"                   FROM filterbyprovincestate b"
                           ,"                   WHERE b.date < a.date"
                           ,"                   AND b.[Province.State] = '", gsub("'", "''", provincestate), "'"
                           ," AND b.Admin2 IS NULL"
                           ,"                   ORDER BY b.date DESC"
                           ,"                   ) Deathsdiff"
                           
                           ,"      , a.Recovered - (SELECT b.Recovered"
                           ,"                   FROM filterbyprovincestate b"
                           ,"                   WHERE b.date < a.date"
                           ,"                   AND b.[Province.State] = '", gsub("'", "''", provincestate), "'"
                           ," AND b.Admin2 IS NULL"
                           ,"                   ORDER BY b.date DESC"
                           ,"                   ) Recovereddiff"
                           
                           ,"      , a.Active - (SELECT b.Active"
                           ,"                   FROM filterbyprovincestate b"
                           ,"                   WHERE b.date < a.date"
                           ,"                   AND b.[Province.State] = '", gsub("'", "''", provincestate), "'"
                           ," AND b.Admin2 IS NULL"
                           ,"                   ORDER BY b.date DESC"
                           ,"                   ) Activediff"
                           
                           
                           ," FROM filterbyprovincestate a WHERE a.[Province.State] = '", gsub("'", "''", provincestate), "'"
                           ," AND a.Admin2 IS NULL"
        )
        filterbyprovinceandstatewithdiff =  sqldf(sqlrequest)
        diffdataframe <- rbind(diffdataframe, filterbyprovinceandstatewithdiff) 
      }
      
      
    } else {
      # The city is specified  
      print(country)  
      for(city in listallcity){
        progress <- n2/length(listallcity)
        percentdone <- paste0(formatC(100 * progress, format = "f", digits = 2), "%")
        print(paste("Progress City:",percentdone))
        n2 <- n2 + 1
        filterbycityandcountry <-  filteredbycountry %>% filter(Admin2 == city)
        cityprovince_state <- sqldf("SELECT DISTINCT([Province.State]) FROM filterbycityandcountry")
        for(province_state in cityprovince_state$Province.State){
          sqlrequest = paste(sep = "", "SELECT a.date, a.[Country.Region], a.[Province.State], a.Admin2, a.Latitude, a.Longitude, a.Confirmed,a.Deaths, a.Recovered, a.Active"
                             ,"      , a.Confirmed - (SELECT b.Confirmed"
                             ,"                   FROM filterbycityandcountry b"
                             ,"                   WHERE b.date < a.date"
                             ,"                   AND b.[Province.State] = '", gsub("'", "''", province_state), "'"
                             ," AND b.Admin2 ='",gsub("'", "''", city),"'"
                             ,"                   ORDER BY b.date DESC"
                             ,"                   ) Confirmeddiff"
                             
                             ,"      , a.Deaths - (SELECT b.Deaths"
                             ,"                   FROM filterbycityandcountry b"
                             ,"                   WHERE b.date < a.date"
                             ,"                   AND b.[Province.State] = '", gsub("'", "''", province_state), "'"
                             ," AND b.Admin2 ='", gsub("'", "''", city) ,"'"
                             ,"                   ORDER BY b.date DESC"
                             ,"                   )  Deathsdiff"
                             
                             ,"      , a.Recovered - (SELECT b.Recovered"
                             ,"                   FROM filterbycityandcountry b"
                             ,"                   WHERE b.date < a.date"
                             ,"                   AND b.[Province.State] = '", gsub("'", "''", province_state), "'"
                             ," AND b.Admin2 ='", gsub("'", "''", city) ,"'"
                             ,"                   ORDER BY b.date DESC"
                             ,"                   )  Recovereddiff"
                             
                             ,"      , a.Active - (SELECT b.Active"
                             ,"                   FROM filterbycityandcountry b"
                             ,"                   WHERE b.date < a.date"
                             ,"                   AND b.[Province.State] = '", gsub("'", "''", province_state), "'"
                             ," AND b.Admin2 ='", gsub("'", "''", city) ,"'"
                             ,"                   ORDER BY b.date DESC"
                             ,"                   )  Activediff"
                             
                             ," FROM filterbycityandcountry a WHERE a.[Province.State] = '", gsub("'", "''", province_state),"'"
                             ," AND a.Admin2 ='", gsub("'", "''", city) ,"'"
          )
          filterbycityandcountrywithdiff =  sqldf(sqlrequest)
          diffdataframe <- rbind(diffdataframe, filterbycityandcountrywithdiff) 
        }
      }
    }
  }
  diffdataframe$Confirmeddiff[is.na(diffdataframe$Confirmeddiff)]<-0
  diffdataframe$Deathsdiff[is.na(diffdataframe$Deathsdiff)]<-0
  diffdataframe$Activediff[is.na(diffdataframe$Activediff)]<-0
  diffdataframe$Recovereddiff[is.na(diffdataframe$Recovereddiff)]<-0
  
  return(diffdataframe)
}

listlocations <- function(dataframe,locationtype = "help",viewlocation = TRUE){
  if(locationtype == "help"){
    print("To list a location you must give the following values to the locationtype parameter: City,State or Country")
    cat("ex: listlocation(mydataframe,locationtype = \"State\"")
    return
  }
  if(locationtype == "City"){
    mylist = dataframe$Admin2
    mylist = sort(unique(mylist))
    if(viewlocation) {View(mylist)}
    return(mylist)
  }
  if(locationtype == "State"){
    mylist = dataframe$Province.State
    mylist = sort(unique(mylist))
    if(viewlocation) {View(mylist)}
    return(mylist)
  }
  if(locationtype == "Country"){
    mylist = dataframe$Country.Region
    mylist = sort(unique(mylist))
    if(viewlocation) {View(mylist)}
    return(mylist)
  }
}

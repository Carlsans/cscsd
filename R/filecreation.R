savefiletoftp <- function(myfile){
 # myfile <- "/Users/carlsansfacon/Desktop/Covid/graphs.tmp/aaa.png"
#myfile = paste(sep="",path,myfile)   
ftppath = paste(sep="","ftp://ftp.moduloinfo.ca/public_html/covid/graphs/",basename(myfile))
print(myfile)
print(ftppath)

  ftpUpload(myfile,
            ftppath,
            userpwd = "ftpgraphs@moduloinfo.ca:mypassword"
  )
}

createfilesystem <- function(basedataframe, only=""){
  
  cleaneddataframe <- basedataframe
  cleaneddataframe$Province.State[cleaneddataframe$Province.State==""]<-NA
  cleaneddataframe$Admin2[cleaneddataframe$Admin2==""]<-NA
  #path = "/Users/carlsansfacon/Desktop/Covid/graphs.tmp/"
  do.call(file.remove, list(list.files(path, full.names = TRUE)))
  
  # Plot country's and save to disk
 
if(only == "" || only == "country"){ 
  listallcountry = listlocations(cleaneddataframe, locationtype = "Country",viewlocation = FALSE)
  for (country in listallcountry){
    afilename = paste(sep="",adaptfilename(paste(sep="",path,"country", country)))
    print(afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Confirmed",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Confirmeddiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Deaths",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Deathsdiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Recovered",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = country,scope = "Country",type = "Recovereddiff",myfilename = afilename)
    
  }
} 
if(only == "" || only == "state"){ 
    
    # Plot states and save to disk
  #filteredbystate <- cleaneddataframe %>% filter(is.na(Admin2))
  filteredbystate <- cleaneddataframe
  listallprovincestate = listlocations(filteredbystate, locationtype = "State",viewlocation = FALSE)
  for (state in listallprovincestate){
    afilename = paste(sep="",adaptfilename(paste(sep="",path,"state", state)))
    print(afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Confirmed",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Confirmeddiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Deaths",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Deathsdiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Recovered",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = state,scope = "State",type = "Recovereddiff",myfilename = afilename)
    
  }
}
  
if(only == "" || only == "city"){ 
  # Plot city and save to disk
  listallcity = listlocations(cleaneddataframe, locationtype = "City",viewlocation = FALSE)
  for(city in listallcity){
    afilename = paste(sep="",adaptfilename(paste(sep="",path,"city", city)))
    print(afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Confirmed",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Confirmeddiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Deaths",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Deathsdiff",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Recovered",myfilename = afilename)
    plotcoviddata(cleaneddataframe,where = city,scope = "City",type = "Recovereddiff",myfilename = afilename)
    
  }
}
  # for (country in listallcountry) {
  #   filteredbycountry <- cleaneddataframe %>% filter(Country.Region == country)
  #   if(all(is.na(filteredbycountry$Province.State)) && all(is.na(filteredbycountry$Admin2))){  
  #     #Country only
  #     myfilename = paste(sep="",adaptfilename(paste(sep="",path,country)),".jpg")
  #     #print(myfilename)
  #     #print(country)
  #     # plotcoviddata(basedataframe,where = country,scope = "Country",type = "Confirmed")
  #     
  #   } else if ( all(is.na(filteredbycountry$Admin2))){
  #     #Province/State only
  #     filterbycountryonly <-  filteredbycountry %>% filter(Country.Region == country && is.na(Admin2) && Province.State != "None" && !is.na(Province.State))
  #     cityprovince_state <- sqldf("SELECT DISTINCT([Province.State]) FROM filterbycountryonly")
  #     for(provincestate in cityprovince_state){
  #       afilename = paste(sep="",adaptfilename(paste(sep="",path,country,provincestate)),".jpg")
  #       #file.create(filename)
  #       print(afilename)
  #       #afilename = ""
  #       #print(provincestate)
  #       if(!is_empty(provincestate)){
  #         plotcoviddata(basedataframe,where = provincestate,scope = "State",type = "Confirmed",myfilename = afilename)
  #       }
  #       
  #       
  #     }
  #   }else{
  #     #City specified
  #     for(city in listallcity){
  #       filterbycityandcountry <-  filteredbycountry %>% filter(Admin2 == city)
  #       cityprovince_state <- sqldf("SELECT DISTINCT([Province.State]) FROM filterbycityandcountry")
  #       for(province_state in cityprovince_state$Province.State)
  #       {
  #         
  #         myfilename = paste(sep="",adaptfilename(paste(sep="",path,country,province_state,city)),".jpg")
  #         
  #         #print(myfilename) 
  #       }
  #       
  #     }
  #   }
  # }
  
}
adaptfilename <- function(myfilename){
  myfilename =  gsub(" ", "_",myfilename)
  myfilename =  gsub(",", "",myfilename)
  myfilename =  gsub("\'", "_",myfilename)
  
  
  
  return(myfilename)
}

createjsonlist <- function(dataframe){
 
  sqlrequest = paste(sep = "", "SELECT DISTINCT [Country.Region],[Province.State], Admin2, Latitude, Longitude FROM dataframe")
  filtereddataframe <- sqldf(sqlrequest)        

  #x <-toJSON(filtereddataframe)
  x <- toJSON(unname(split(filtereddataframe, 1:nrow(filtereddataframe))))
  write(x, paste(sep="", path, "csclist.json" ))
  #cat(x)
  
  
}

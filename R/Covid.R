# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# Author : Carl Sansfaçon

## This code download all the latest result from the 
## 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository
## and reformat them to fit a dataframe that contain all the results.
## The 2 last sample functions do simple arima prediction on them. 


# puth the path where you've copied your source files
sourcepath = "/Users/carlsansfacon/Desktop/Covid/R/"
# path where you want the automated graphic file system to be saved
path <<- "/Users/carlsansfacon/Desktop/Covid/graphs.tmp/"

# Comment the 2 following lines after installation
source(paste(sep = "",sourcepath,"installpackage.R"))
installallpackage()

source(paste(sep = "",sourcepath,"covidimports.R"))
source(paste(sep = "",sourcepath,"dataacquisition.R"))
source(paste(sep = "",sourcepath,"plotting.R"))
source(paste(sep = "",sourcepath,"filecreation.R"))





refreshfilesystem<- function(){
  ftp <<- FALSE
  
  start_time <- Sys.time()

  h2o.init(nthreads = -1 )
  dataframe = downloadlatestdata(today = TRUE)
  diffdataframe <- getdifferencialvalues(dataframe)
  
  createfilesystem(diffdataframe)
  end_time <- Sys.time()
  
  createjsonlist(diffdataframe)
  print(end_time-start_time) 
  
}
# Create the whole file system. Take about 2-3 hours on intel i7 6 core 2.4-3.2
refreshfilesystem()
h2o.shutdown

example <- function(){
plotcoviddata(diffdataframe,where = "Delaware",scope = "State",type = "Confirmed",myfilename = "/Users/carlsansfacon/Desktop/Covid/test1")
plotcoviddata(diffdataframe,where = "Quebec",scope = "State",type = "Confirmed",myfilename = "/Users/carlsansfacon/Desktop/Covid/test1")
plotcoviddata(cleaneddataframe,where = "District of Columbia",scope = "State",type = "Confirmed",myfilename = "/Users/carlsansfacon/Desktop/Covid/columbiatest1")
}









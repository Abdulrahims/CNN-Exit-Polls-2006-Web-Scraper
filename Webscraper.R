require(rvest)
remove(list=ls())

#States for senate races
states_senate <- c("AZ","CA","CT","FL","HI","ME","MD","MA","MI","MN","MO","MT","NE","NV","NJ","NM","NY","ND","OH","PA","RI","TN","TX","UT","VT","VA","WA","WV","WI","WY")

#The CSS element of the tables is dataTable
selector_tables<-".dataTable"

#This data frame will contain all the scraped data at the end of the loop
polls=data.frame()

#Senate polls scraper:

for(i in states_senate){
  url <- paste("http://www.cnn.com/ELECTION/2006/pages/results/states/",i,"/S/01/epolls.0.html",sep="")
  
  #This is the scraper that creates a list from all of the elements matching 'dataTable' on the webpage
  tables<- url %>%
    read_html %>%
    html_nodes(selector_tables) %>%
    html_table()
  
  #This will pull all the text on the webpage, but we only want the number of respondents, so we will subset it later
  sampsize <- url %>%
    read_html %>%
    html_nodes("td") %>%
    html_text()
  
  #Subset to the line in the list that contains the number of respondents
  sampsize <- sampsize[74]
  
  #check[2] will return either "demHdr" or "repHdr," depending on which appears first. If demHdr appears first,
  #this means that a democrat won that state
  
  check <- url %>%
    read_html %>%
    html_nodes("th") %>%
    html_attrs()
  
  #The scraper pulls all 30 tables on the webpage, but we only want a few of those
  #Florida has a slightly different format (they have Cuban as an option for race), so we need an exception
  if(i=='FL'){
    
    gender <- tables[[1]]
    gender <- gender[1:3]
    gender$var='gender'

    race <- tables[[3]]
    race <- race[1:3]
    race$var='race'
    
    age <- tables[[5]]
    age <- age[1:3]
    age$var='age'
    
    income <- tables[[7]]
    income <- income[1:3]
    income$var='income'
    
  }else{
    
    gender <- tables[[1]]
    gender <- gender[1:3]
    gender$var='gender'
    
    race <- tables[[3]]
    race <- race[1:3]
    race$var='race'
    
    age <- tables[[4]]
    age <- age[1:3]
    age$var='age'
    
    income <- tables[[6]]
    income <- income[1:3]
    income$var='income'
  }
  
  all <- rbind(gender,income,race,age)
  all$winner <- check[2]
  rm(gender,income, race, age)
  
  #Formatting the table
  all$state=i
  all$year = 2006
  all$office = "senate"
  all$pctofelectorate <- NA
  all$samplesize <- gsub('Respondents.*', '', sampsize)

  colnames(all)[2]='incumbent'
  colnames(all)[3]='challenger'
  colnames(all)[1] <- "cat"

  message(i)
  
  polls = rbind(polls,all)
  
  rm(all,check,tables)
}

#Retrieving the percentages within the parenthesis and placing them in the pctofelectorate column
for(i in 1:nrow(polls)){
  polls$pctofelectorate[i] <- gsub(".*\\((.*)\\).*", "\\1", polls$cat[i])
}

#Removing the parenthesis
polls$cat<-gsub("\\s*\\([^\\)]+\\)","",as.character(polls$cat))

#Removing the percent signs
polls$incumbent<-gsub("\\s*\\%","",as.character(polls$incumbent))
polls$challenger<-gsub("\\s*\\%","",as.character(polls$challenger))
polls$pctofelectorate <- gsub("\\s*\\%","",as.character(polls$pctofelectorate))

#Formatting:

#Using the winner column to determine the REP and DEM totals
#incumbent will become the REP column
#challanger will become the DEM column

for(i in 1:570){
  if(polls$winner[i] == "demHdr"){
    
    #swap with the challanger column
    temp = polls$incumbent[i]
    polls$incumbent[i] = polls$challenger[i]
    polls$challenger[i] = temp
  }
}

#Then, rename the columns:
colnames(polls)[2]='REP'
colnames(polls)[3]='DEM'

#Remove the winner column
polls <- polls[-5]

#Reorder the data frame in the same order as the SQL table

polls <- data.frame(polls$state, polls$year, polls$office, polls$var, polls$cat, polls$pctofelectorate, polls$DEM,polls$REP,polls$samplesize)
colnames(polls)[1]='STATE'
colnames(polls)[2]='YEAR'
colnames(polls)[3]='OFFICE'
colnames(polls)[4]='VAR'
colnames(polls)[5]='CAT'
colnames(polls)[6]='PCTOFELECTORATE'
colnames(polls)[7]='DEM'
colnames(polls)[8]='REP'
colnames(polls)[9]='SAMPSIZE'

#Recoding the variables to the names in the SQL table as much as possible:

polls$CAT <- as.character(polls$CAT)

for(i in 1:570){
  if(polls$CAT[i] == "Male"){
    polls$CAT[i] = "M"
  }else if(polls$CAT[i] == "Female"){
    polls$CAT[i] = "F" 
  }else if(polls$CAT[i] == "African-American"){
    polls$CAT[i] = "B"
  }else if(polls$CAT[i] == "Asian"){
    polls$CAT[i] = "A"
  }else if(polls$CAT[i] == "White"){
    polls$CAT[i] = "W"
  }else if(polls$CAT[i] == "Latino"){
    polls$CAT[i] = "H"
  }else if(polls$CAT[i] == "Under $15,000"){
    polls$CAT[i] = "Under 15k"
  }else if(polls$CAT[i] == "$15-30,000"){
    polls$CAT[i] = "15-30k"
  }else if (polls$CAT[i] == "$30-50,000"){
    polls$CAT[i] = "30-50k"
  }else if (polls$CAT[i] == "$50-75,000"){
    polls$CAT[i] = "50-75k"
  }else if (polls$CAT[i] == "$75-100,000"){
    polls$CAT[i] = "75-100k"
  }else if (polls$CAT[i] =="$100-150,000"){
    polls$CAT[i] = "100-150k"
  }else if (polls$CAT[i] == "$150-200,000"){
    polls$CAT[i] = "150-200k"
  }else if(polls$CAT[i] == "$200,000 or More"){
    polls$CAT[i] = "200k or more"
  }else{}
}

#Governor races scraper:

govpolls=data.frame()
 
#States for the governor races
states_gov <- c("AZ","CA","CT","FL","GA","HI","IL","ME","MD","MA","MI","MN","NE","NV","NM","NY","OH","PA","RI","TN","TX","VT","WI","WY")

for(i in states_gov){
  url <- paste("http://www.cnn.com/ELECTION/2006/pages/results/states/",i,"/G/00/epolls.0.html",sep="")
  
  #This is the scraper that creates a list from all of the elements matching 'dataTable' on the webpage
  tables<- url %>%
    read_html %>%
    html_nodes(selector_tables) %>%
    html_table()
  
  #This will pull all the text on the webpage, but we only want the number of respondents, so we will subset it later
  sampsize <- url %>%
    read_html %>%
    html_nodes("td") %>%
    html_text()
  
  #Subset to the line in the list that contains the number of respondents
  sampsize <- sampsize[74]
  
  #check[2] will return either "demHdr" or "repHdr," depending on which appears first. If demHdr appears first,
  #this means that a democrat won that state
  
  check <- url %>%
    read_html %>%
    html_nodes("th") %>%
    html_attrs()
  
  #The scraper pulls all 30 tables on the webpage, but we only want a few of those
  #Florida has a slightly different format (they have Cuban as an option for race), so we need an exception
  if(i=='FL'){
    
    gender <- tables[[1]]
    gender <- gender[1:3]
    gender$var='gender'
    
    race <- tables[[3]]
    race <- race[1:3]
    race$var='race'
    
    age <- tables[[5]]
    age <- age[1:3]
    age$var='age'
    
    income <- tables[[7]]
    income <- income[1:3]
    income$var='income'
    
  }else{
    
    gender <- tables[[1]]
    gender <- gender[1:3]
    gender$var='gender'
    
    race <- tables[[3]]
    race <- race[1:3]
    race$var='race'
    
    age <- tables[[4]]
    age <- age[1:3]
    age$var='age'
    
    income <- tables[[6]]
    income <- income[1:3]
    income$var='income'
  }
  
  all <- rbind(gender,income,race,age)
  all$winner <- check[2]
  rm(gender,income, race, age)
  
  #Formatting the table
  all$state=i
  all$year = 2006
  all$office = "gov"
  all$pctofelectorate <- NA
  all$samplesize <- gsub('Respondents.*', '', sampsize)
  
  colnames(all)[2]='incumbent'
  colnames(all)[3]='challenger'
  colnames(all)[1] <- "cat"
  
  message(i)
  
  govpolls = rbind(govpolls,all)

  rm(all,check,tables)
}

#Retrieving the percentages within the parenthesis and placing them in the pctofelectorate column
for(i in 1:nrow(govpolls)){
  govpolls$pctofelectorate[i] <- gsub(".*\\((.*)\\).*", "\\1", govpolls$cat[i])
}

#Removing the parenthesis
govpolls$cat<-gsub("\\s*\\([^\\)]+\\)","",as.character(govpolls$cat))

#Removing the percent signs
govpolls$incumbent<-gsub("\\s*\\%","",as.character(govpolls$incumbent))
govpolls$challenger<-gsub("\\s*\\%","",as.character(govpolls$challenger))
govpolls$pctofelectorate <- gsub("\\s*\\%","",as.character(govpolls$pctofelectorate))

#Formatting:

#Using the winner column to determine the REP and DEM totals
#incumbent will become the REP column
#challanger will become the DEM column

for(i in 1:456){
  if(govpolls$winner[i] == "demHdr"){
    
    #swap with the challanger column
    temp = govpolls$incumbent[i]
    govpolls$incumbent[i] = govpolls$challenger[i]
    govpolls$challenger[i] = temp
  }
}

#Then, rename the columns:
colnames(govpolls)[2]='REP'
colnames(govpolls)[3]='DEM'

#Remove the winner column
govpolls <- govpolls[-5]

#Reorder the data frame in the same order as the SQL table

govpolls <- data.frame(govpolls$state, govpolls$year, govpolls$office, govpolls$var, govpolls$cat, govpolls$pctofelectorate, govpolls$DEM,govpolls$REP,govpolls$samplesize)
colnames(govpolls)[1]='STATE'
colnames(govpolls)[2]='YEAR'
colnames(govpolls)[3]='OFFICE'
colnames(govpolls)[4]='VAR'
colnames(govpolls)[5]='CAT'
colnames(govpolls)[6]='PCTOFELECTORATE'
colnames(govpolls)[7]='DEM'
colnames(govpolls)[8]='REP'
colnames(govpolls)[9]='SAMPSIZE'

#Recoding the variables to the names in the SQL table as much as possible:

govpolls$CAT <- as.character(govpolls$CAT)

for(i in 1:456){
  if(govpolls$CAT[i] == "Male"){
    govpolls$CAT[i] = "M"
  }else if(govpolls$CAT[i] == "Female"){
    govpolls$CAT[i] = "F" 
  }else if(govpolls$CAT[i] == "African-American"){
    govpolls$CAT[i] = "B"
  }else if(govpolls$CAT[i] == "Asian"){
    govpolls$CAT[i] = "A"
  }else if(govpolls$CAT[i] == "White"){
    govpolls$CAT[i] = "W"
  }else if(govpolls$CAT[i] == "Latino"){
    govpolls$CAT[i] = "H"
  }else if(govpolls$CAT[i] == "Under $15,000"){
    govpolls$CAT[i] = "Under 15k"
  }else if(govpolls$CAT[i] == "$15-30,000"){
    govpolls$CAT[i] = "15-30k"
  }else if (govpolls$CAT[i] == "$30-50,000"){
    govpolls$CAT[i] = "30-50k"
  }else if (govpolls$CAT[i] == "$50-75,000"){
    govpolls$CAT[i] = "50-75k"
  }else if (govpolls$CAT[i] == "$75-100,000"){
    govpolls$CAT[i] = "75-100k"
  }else if (govpolls$CAT[i] =="$100-150,000"){
    govpolls$CAT[i] = "100-150k"
  }else if (govpolls$CAT[i] == "$150-200,000"){
    govpolls$CAT[i] = "150-200k"
  }else if(govpolls$CAT[i] == "$200,000 or More"){
    govpolls$CAT[i] = "200k or more"
  }else{}
}

#Merge polls and govpolls:

exitpolls <- rbind(polls,govpolls)

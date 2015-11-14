library(shiny)
library(wordcloud)
library(stringr)
library(knitr)
library(selectr)
library(data.table)
library(RCurl)
library(RJSONIO)
library(plyr)
require(ggplot2)
library(sp)
library(tm)
library(quanteda)
library(NLP)
library(devtools)
dev_mode(on=TRUE)
library(gender)
library(jsonlite)
library(scales)
######################################################################################
gender_profile <- function(team_profiles) {
  names <- word(team_profiles$profile_name,1)
  team_genders <- gender(names,method = "ssa",years = c(1900, 2012))
  return(c(sum(team_genders$gender=="male"),sum(team_genders$gender=="female")))
}
######################################################################################
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
######################################################################################
clean_data <- function(data) {
  data <- sapply(data, as.character)
  data[is.na(data)] <- ""
  data <- as.data.frame(data)
  return(data)
}
#######################################################################################
check_empty <- function(data) {
  percentage_empty <- as.data.frame(colnames(data))
  for (i in 1:ncol(data)) {
    percentage_empty[i,2] <- 100*length(data[data[,i]=="",i])/length(data[,i])
  }
  colnames(percentage_empty) <- c("field",paste("% empty"," ",company,sep = ""))
  return(percentage_empty)
}
########################################################################################
stem_colleges <- function(team_profiles) {
  for (i in 1:5) {
    team_profiles[,13+i] <- tolower(team_profiles[,13+i])
    team_profiles[,13+i] <- gsub("[[:punct:]]","" ,team_profiles[,13+i])
  }
  return(team_profiles)
}
########################################################################################
identify_team <- function(data,team_keywords) {
  team_binary <- rep(0,nrow(data))
  for (i in 1:length(team_keywords)) {
    team_binary <- team_binary + grepl(team_keywords[i], data$current_title, ignore.case = TRUE)
  }
  team_binary <- sign(team_binary)
  team_profiles <- data[as.logical(team_binary),]
  if (nrow(team_profiles)==0)  {
    return("Keywords not found! Team is empty.")
  }
  return(team_profiles)
}
#########################################################################################
stem_degrees <- function(team_profiles) {
  for (i in 1:5) {
    team_profiles[,3+i] <- tolower(team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("[[:punct:]]","" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("masters","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("ms ","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("ma ","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("mba ","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("mphil","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("msc ","master" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("bsc ","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("btech","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("ba ","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("bs","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("bachelors","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("ab","bachelor" ,team_profiles[,3+i])
    team_profiles[,3+i] <- gsub("dphil","phd" ,team_profiles[,3+i])
    team_profiles[grepl("phd",team_profiles[,3+i]),3+i] <- "phd"
    team_profiles[grepl("master",team_profiles[,3+i]),3+i] <- "master"
    team_profiles[grepl("bachelor",team_profiles[,3+i]),3+i] <- "bachelor"
  }
  return(team_profiles)
}
############################################################################
degree_count <- function(team_profiles) {
  highest_degree <- c()
  for (ii in 1:nrow(team_profiles)) {
    a <- max(grepl("phd",rapply(team_profiles[ii,4:8],as.character), ignore.case = TRUE))>0
    b <- max(grepl("master",rapply(team_profiles[ii,4:8],as.character), ignore.case = TRUE))>0 
    c <- max(grepl("bachelor",rapply(team_profiles[ii,4:8],as.character), ignore.case = TRUE))>0
    ifelse(a,highest_degree[ii] <- "PhD",
           ifelse(b,highest_degree[ii] <- "Master",
                  ifelse(c,highest_degree[ii] <- "Bachelor",highest_degree[ii] <- "Other")))
  }
  highest_degree <- as.data.frame(highest_degree)
  return(as.data.frame(table(highest_degree)))
}
#############################################################################
skill_analysis <- function(team_profiles) {
  temp <- strsplit(as.character(team_profiles$skills), ",", fixed = TRUE, perl = FALSE, useBytes = FALSE)
  temp <- rapply(temp,unlist)
  temp <- tolower(temp)
  temp <- gsub("[[:punct:]]","" ,temp)
  temp <- paste(ifelse(substr(temp, 1, 1)==" ","",substr(temp, 1, 1)), substr(temp, 2, nchar(temp)), sep="")
  temp <- paste(toupper(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)), sep="")
  return(as.data.frame(table(temp)))
}
##############################################################################
stem_fields <- function(team_profiles) {
  for (i in 1:5) {
    team_profiles[,8+i] <- tolower(team_profiles[,8+i])
    team_profiles[grepl("engineering",team_profiles[,8+i]),8+i] <- "Engineering"
    team_profiles[grepl("math",team_profiles[,8+i]),8+i] <- "Math/statistics"
    team_profiles[grepl("statistics",team_profiles[,8+i]),8+i] <- "Math/statistics"
    team_profiles[grepl("physics",team_profiles[,8+i]),8+i] <- "Physics"
    team_profiles[grepl("computer",team_profiles[,8+i]),8+i] <- "Computer Science"
    team_profiles[grepl("systems",team_profiles[,8+i]),8+i] <- "Computer Science"
    team_profiles[grepl("computational",team_profiles[,8+i]),8+i] <- "computer science"
    team_profiles[grepl("market",team_profiles[,8+i]),8+i] <- "Marketing"
    team_profiles[grepl("business",team_profiles[,8+i]),8+i] <- "Finance/economics"
    team_profiles[grepl("computing",team_profiles[,8+i]),8+i] <- "Computer Science"
    team_profiles[grepl("financial",team_profiles[,8+i]),8+i] <- "Finance/economics"
    team_profiles[grepl("finance",team_profiles[,8+i]),8+i] <- "Finance/economics"
    team_profiles[grepl("economics",team_profiles[,8+i]),8+i] <- "Finance/economics"
    team_profiles[grepl("account",team_profiles[,8+i]),8+i] <- "Finance/economics"
    team_profiles[grepl("data",team_profiles[,8+i]),8+i] <- "Finance/economics"
  }
  return(team_profiles)
}
#################################################################################
time_in_role <- function(team_profiles) {
  time <- as.character(team_profiles$d1)
  time <- time[time != ""]
  for (i in 1:length(time)) {
    time[i] <- gsub(" ","" ,time[i])
    time[i] <- gsub("s","" ,time[i])
    time[i] <- gsub("[\\(\\)]", "", regmatches(time[i], gregexpr("\\(.*?\\)", time[i]))[[1]])
    if (grepl("l",time[i],ignore.case = TRUE)) {
      time[i] <- 5 # average of year to date
    }
    else if ( grepl("y",time[i],ignore.case = TRUE) & grepl("m",time[i],ignore.case = TRUE) ) {
      time[i] <- as.numeric(substring(time[i],1,1))*12 + as.numeric(substr(time[i],6,6))
    }
    else if ( grepl("y",time[i],ignore.case = TRUE) & !grepl("m",time[i],ignore.case = TRUE)) {
      time[i] <- as.numeric(substring(time[i],1,1))*12
    }
    else if ( !grepl("y",time[i],ignore.case = TRUE) & grepl("m",time[i],ignore.case = TRUE)) {
      time[i] <- as.numeric(substring(time[i],1,1))
    }
    
  }
  months_in_company <- as.numeric(time)
  return(months_in_company/12)
}
##################################################################################

# Define server logic 
shinyServer(function(input, output) {  
  dataInput <- reactive({
    filename <- paste(as.character(tolower(input$company)),"_DS.csv",sep = "")
    #filename <- paste(as.character(input$company),"_DS.csv",sep = "")
    data <- read.csv(filename)
    #data$X <- NULL
    data <- clean_data(data)
    #keywords <- c("data science","data scientist","data mining","machine learning")
    #team_profiles <- identify_team(data,keywords)
  })
  output$summary <- renderDataTable({
    team_profiles <- dataInput()
    company <- as.character(tolower(input$company))
    start_request <- "http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=44360&t.k=bBEoaEGjpLk&action=employers&q="
    end_request <- "&userip=79.97.106.19&useragent=Mozilla/%2F4.0"
    api_request <- paste(start_request,company,end_request,sep = "")
    if (nrow(team_profiles)*1.2 <= 10) {
      size_team = "Small (<10)"
    }
    if (nrow(team_profiles)*1.2 > 10) {
      size_team = "Medium (10-25)"
    }
    if (nrow(team_profiles)*1.2 > 25) {
      size_team = "Large (25-50)"
    }
    if (nrow(team_profiles)*1.2 > 50) {
      size_team = "Very Large (>50)"
    }
    z <- try(fromJSON(api_request),silent = TRUE)
    ifelse(inherits(z,"try-error"),{
      closeAllConnections()
      company_summary <- data.frame(Industry=" no information",
                                    EmployeeRating=" none",
                                    Description=" none",Ratings=" no ratings",DataScience=size_team)},
      {
        industry <- z$response$employers$industry[1]
        rating <- z$response$employers$overallRating[1]
        rating_desc <- z$response$employers$ratingDescription[1]
        num_ratings <- z$response$employers$numberOfRatings[1]
        
        company_summary <- data.frame(Industry=industry,
                                      EmployeeRating=rating,
                                      Description=rating_desc,Ratings=num_ratings,DataScience=size_team)
      })
    company_summary
  }, options = list(ordering=0,searching=FALSE,paging=FALSE,scrollCollapse=TRUE,info=FALSE))
  output$skills <- renderPlot({
    team_profiles <- dataInput()
    # skill analysis
    skill_freq <- skill_analysis(team_profiles)
    colnames(skill_freq) <- c("skill","Freq")
    skill_freq$skill <- factor(skill_freq$skill, levels = unique(skill_freq$skill[order(skill_freq$Freq)]))
    skill_freq <- skill_freq[with(skill_freq, order(-Freq)),]
    # create plot object
    ggplot(skill_freq[1:10,],aes(x=skill,y=Freq)) + geom_bar(colour="black", fill="#53C253",stat = 'identity') + coord_flip()+ ggtitle("Team Skills") + theme(title=element_text(colour="white"),
                                                                                                                                                              axis.title.x=element_blank(), 
                                                                                                                                                              axis.text.x=element_text(colour="white"),
                                                                                                                                                              axis.text.y=element_text(colour="white"),
                                                                                                                                                              axis.title.y=element_blank(),
                                                                                                                                                              plot.background=element_blank())
  },bg="transparent")
  output$time_in_company <- renderPlot({
    team_profiles <- dataInput()
    # time in company analysis
    time <- data.frame(time_months=time_in_role(team_profiles))
    #team <- as.data.frame(rep("Data Science",length(time)))
    #colnames(team) <- c("team")
    #duration <- as.data.frame(cbind(time,team))
    p <- ggplot(time,aes(time_months)) + geom_histogram(colour="black",fill="#53C253",alpha = 1,position = 'identity',binwidth=0.25)
    p + ggtitle("Duration in Current Position (years)") + theme(title=element_text(colour="white"),plot.background=element_blank(),axis.title.x=element_text(colour="white"),axis.title.y=element_text(colour="white"),axis.text.x=element_text(colour="white"),axis.text.y=element_text(colour="white")) + xlab("Time (in years)") + ylab("Number of Employees") + scale_y_continuous(breaks=pretty_breaks())+ scale_x_continuous(breaks=pretty_breaks())
  },bg="transparent")
  output$colleges <- renderPlot({
    team_profiles <- dataInput()
    #team_profiles <- stem_colleges(team_profiles)
    mytext <- do.call("rbind", list(as.character(team_profiles[,14]), as.character(team_profiles[,15]),
                                    as.character(team_profiles[,16]),as.character(team_profiles[,17]),as.character(team_profiles[,18])))
    x=tokenize(toLower(mytext), removePunct = TRUE, ngrams = 2)
    y=tokenize(toLower(mytext), removePunct = TRUE, ngrams = 3)
    z=tokenize(toLower(mytext), removePunct = TRUE, ngrams = 4)
    x <- mapply(c,x, y, SIMPLIFY=FALSE)
    x <- mapply(c,x, z, SIMPLIFY=FALSE)
    x <- unlist(x)
    x <- as.data.frame(table(x))
    x$x <- gsub("_"," ",x$x)
    unis <- read.csv("unis_clean.csv")
    x <- x[x$x %in% unis$x,]
    number_degrees <- x[with(x,order(-Freq)),]
    number_degrees[grepl("massachusetts institute of technology",number_degrees$x,ignore.case = TRUE),1] <- "MIT"
    number_degrees$x <- sapply(number_degrees$x,simpleCap)
    pal2 <- brewer.pal(8,"Dark2")
    wordcloud(number_degrees$x,number_degrees$Freq,use.r.layout=FALSE,fixed.asp = FALSE,
              random.order = FALSE,rot.per = 0,max.words = 6,scale = c(3,1),colors = pal2)
  },bg="transparent")
  output$highest_degree <- renderPlot({
    team_profiles <- dataInput()
    #team_profiles <- stem_degrees(team_profiles)
    #highest degrees analysis
    count_highest_degrees <- degree_count(team_profiles) 
    colnames(count_highest_degrees) <- c("highest_degree","Freq")
    count_highest_degrees$highest_degree <- factor(count_highest_degrees$highest_degree, levels = count_highest_degrees$highest_degree[order(count_highest_degrees$Freq)])
    ggplot(count_highest_degrees,aes(x=highest_degree,y=Freq,fill=highest_degree)) + geom_bar(colour="black", fill="#53C253",stat = 'identity') + coord_flip() + ggtitle("Highest Degree Obtained")+ theme(title=element_text(colour="white"),
                                                                                                                                                                                                           axis.title.x=element_blank(), 
                                                                                                                                                                                                           axis.title.y=element_blank(),
                                                                                                                                                                                                           axis.text.x=element_text(colour="white"),
                                                                                                                                                                                                           axis.text.y=element_text(colour="white"),
                                                                                                                                                                                                           plot.background=element_blank())
  },bg="transparent")
  output$field_of_study <- renderPlot({
    team_profiles <- dataInput()
    #team_profiles <- stem_fields(team_profiles)
    x <- unlist(team_profiles[,9:13])
    fields <- as.data.frame(table(x))
    colnames(fields) <- c("field","Freq")
    fields <- fields[fields$field!="",]
    fields <- fields[with(fields, order(-Freq)),]
    fields <- fields[1:4,]
    fields$field <- sapply(as.character(fields$field),simpleCap)
    fields$field <- factor(fields$field, levels = fields$field[order(fields$Freq)])
    ggplot(fields,aes(x=field,y=Freq,fill=field)) + geom_bar(colour="black", fill="#53C253",stat = 'identity') + coord_flip() + ggtitle("Fields of Study (by number of degrees)") + theme(title=element_text(colour="white"),
                                                                                                                                                                                          axis.title.x=element_blank(),
                                                                                                                                                                                          axis.title.y=element_blank(),
                                                                                                                                                                                          axis.text.x=element_text(colour="white"),
                                                                                                                                                                                          axis.text.y=element_text(colour="white"),
                                                                                                                                                                                          plot.background=element_blank())
  },bg="transparent")
  output$gender <- renderPlot({
    team_profiles <- dataInput()
    values=gender_profile(team_profiles)
    labels=c("Male", "Female")
    colors=c("#df691a","#53C253")
    percent_str <- paste(round(values/sum(values),2)*100, "%", sep="")
    values <- data.frame(val = values, Type = labels, percent=percent_str )
    pie <- ggplot(values, aes(x = "", y = val, fill = Type)) + 
      geom_bar(colour="black",stat="identity",width = 1) + 
      geom_text(aes(y = val/2 + c(0, cumsum(val)[-length(val)]), label = percent), size=10) + ggtitle("Gender Split Percentages")
    pie + coord_polar(theta = "y")+ scale_fill_manual(values = c("#df691a","#53C253"))+ theme(title=element_text(colour="white"),
                                                                                              panel.grid = element_blank(),
                                                                                              axis.title.x=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.text=element_blank(), 
                                                                                              axis.ticks=element_blank(),
                                                                                              plot.background=element_blank())
    
  },bg="transparent")
})

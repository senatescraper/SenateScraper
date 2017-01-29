library(rvest)
library(stringr)
library(plyr)
library(dplyr)

gitpath<-"***/SenateScraper" #Set to your repository

SenPhones <- function(sen.website) {

  webname <-paste(sen.website)
  website <-  html_text(read_html(webname)) #pull in website address
  website <-gsub("\\r","",website) #Clean up text, get rid of breaks
  website <-gsub("\\n","",website)
  website <-gsub("\\t","",website)
  website<-str_to_upper(website) 
  fax <- unlist(str_extract_all(website,"FAX.+?[A-z]"))  #Find faxes and TTY PHONES. 
  fax2 <-unlist(str_extract_all(website,"F.+?[A-z]", ""))
  tty <- unlist(str_extract_all(website,"TY.+?[A-z]", ""))
  # website <-str_replace_all(website,"FAX.+?[A-z]", "") #Get rid of fax #s
  # website <-str_replace_all(website,"F.+?[A-z]", "") #Get rid of fax #s for "F:"
  # website <-str_replace_all(website,"TY:.+?[A-z]", "") #Get rid of TTY #s 
  
  phone <-"((\\(\\d{3}\\) ?)|(\\d{3}-))?\\d{3}-\\d{4}|([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})|[.(]?\\d{3}[-.)]?[\\s]?\\d{3}[.]?[\\s]?[-.]?[\\s]?[.]?\\d{4}"
  
  
  sen.phones <- unlist(str_extract_all(website,phone))
  sen.phones <- unique(sen.phones)
  sen.phones <- str_replace_all(sen.phones, "[^[:alnum:]]", "")
  fax <-c(fax,fax2,tty)
  fax<-str_replace_all(fax, "[^0-9]", "") 
  sen.phones <- sen.phones[str_length(sen.phones)>=10]
  sen.phones <- setdiff(sen.phones,fax) #Remove fax numbers and TTY
  sen.phones[substr(sen.phones,1,3)!=202] #Get rid of the DC Number 
}

  sen.info <-read.csv(paste(gitpath,"/Input/Senator Basic Info.csv",sep=""), stringsAsFactors=FALSE) 
 
  sen.numbers<-data.frame() #Initialize
  
  for (i in(1:100)) {
    sen <- SenPhones(sen.info$Contact.Site[i])
    sen <- as.data.frame(cbind(sen.info$Last[i],t(sen)))   
    if(sen.info$Last[i]=="LEE"){
      #Manual Add in for Lee (has no webpage with all numbers does not work)
      sen <-as.data.frame(cbind(sen.info$Last[i],t(c("4356285514","8013929633","8015245933"))))
    }
    
    sen.numbers <-rbind.fill(sen.numbers,sen)
  }
  
names(sen.numbers)<-paste("Local.Phone",names(sen.numbers),sep="")

sen.info.numbers <- merge(sen.info,sen.numbers, by.x="Last", by.y="Local.PhoneV1")  

write.csv(sen.info.numbers,file=paste(gitpath,"/Output/Senate Local Numbers.csv",sep="")) #CSV With numbers

# #Get Sentator Websites - Can be used to pull main senate site from sent page.
# all.sens <- read_html("https://www.senate.gov/general/contact_information/senators_cfm.xml")
# all.sens <- html_text(all.sens)
# 
# name <- "www.*(?=\\n.*Class)"  #Regex to pull website (based on pattern www.[website] \n   Class)
# test.name<-str_extract_all(all.sens,regex(pattern=name)) #Pull website
# test.name<-as.data.frame(test.name)  #Make dataframe
# colnames(test.name) <- "site"  
# test.name$URL <-paste("http://",test.name$site,sep="")
# test.name$Senator <-str_extract(test.name$URL,"(?<=www.).*(?=.sen)")  #Extract Senator Name 
# write.csv(test.name,file="senators.csv")

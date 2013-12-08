library(plyr)
library(stringr)
library(ggplot2)

inventory <- read.csv("inventory_extended.csv")

democracy.mentions <- data.frame(filename=NA,mentions=NA)
democracy.mentions <- na.omit(democracy.mentions)

path <- "C:/Users/Marquez family/Dropbox/Downloading Constitutions/" #Path of downloaded constitutions

for(f in paste(path,"ccp/",inventory$full.filename.txt,sep="")) {
  fp <- readLines(file(f, open="rt"))
  lines.matching <- grep("democ|demok|mocra|demo-$", fp, ignore.case=TRUE)
  print(f)
  lines.matching <- sort(c(lines.matching,lines.matching-1,lines.matching+1))
  mention <- paste(fp[lines.matching],collapse="/")
  #print(fp[lines.matching])
  mentions <- data.frame(filename=f,mentions=mention)
  democracy.mentions<- rbind(democracy.mentions, mentions)
}
rm(mentions,i,fp,f,lines.matching)
#democracy.mentions <- unique(democracy.mentions)
democracy.mentions$mentions <- str_trim(democracy.mentions$mentions)
democracy.mentions$mentions <- ifelse(democracy.mentions$mentions == "",NA,as.character(democracy.mentions$mention))

democracy.mentions <- merge(democracy.mentions, inventory)
rm(inventory)
democracy.mentions <- democracy.mentions[ , !(names(democracy.mentions) %in% c("Full.Description","Caption","File.Format","Last.Update.by.User","Last.Update.on.Date","Link.To.Repository.Entry","original.filename","to.ocr","isdir","mode","mtime","ctime","atime","exe","Is.Public.","COPYRIGHT","PUBLISHER_LOCATION","full.filename"))]
democracy.mentions <- rename(democracy.mentions, c("size"="txt.size"))

#fixing some mistakes
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Australia" ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Norway" & democracy.mentions$YEAR == 1938 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Sweden" & democracy.mentions$YEAR == 1809 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Wurttemberg" & democracy.mentions$YEAR == 1819 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Haiti" & democracy.mentions$YEAR == 1843 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Mexico" & democracy.mentions$YEAR == 1846 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Haiti" & democracy.mentions$YEAR == 1846 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Haiti" & democracy.mentions$YEAR == 1849 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Sweden" & democracy.mentions$YEAR == 1866 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Yugoslavia (Serbia)" & democracy.mentions$YEAR == 1901 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Ukraine" & democracy.mentions$YEAR == 1991 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Netherlands" & democracy.mentions$YEAR == 1798 ] <- NA
democracy.mentions$mentions[ democracy.mentions$COUNTRY == "Spain" & democracy.mentions$YEAR == 1869 ] <- NA

write.csv(democracy.mentions,"democracy.mentions.csv",row.names=FALSE)

# Mentions
democracy.mentions$has.mention <- !is.na(democracy.mentions$mentions)
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY,YEAR), transform, year.has.mention = any(has.mention))

#Languages captured 
unique(democracy.mentions$language[democracy.mentions$has.mention]) #21 languages

#files that do not mention democracy
summary(democracy.mentions[!democracy.mentions$has.mention,])

#1539 mention democracy, 2355 do not, including the 52 with no text; of these, 391 are new constitutions, 38 are interim, 1877 are amendments, 27 are "reinstated" (and could actually mention democracy in the previous version), 18 are suspensions

summary(democracy.mentions[ !democracy.mentions$has.mention & democracy.mentions$EVENTTYPE=="new",c("Name","COUNTRY","YEAR")]) #Mostly very old ones- mean year 1873

nrow(democracy.mentions[ !democracy.mentions$has.mention & democracy.mentions$EVENTTYPE=="new" & democracy.mentions$YEAR < 1848,c("Name","COUNTRY","YEAR")]) #Constitutions to check to see if there's anything older than France 1848 that mentions democracy; 161 of them

democracy.mentions$COUNTRY <- mapvalues(democracy.mentions$COUNTRY, from=c("Morroco", "Korea, People'S Republic Of","Korea, Republic Of","Yemen, People'S Republic Of"), to=c("Morocco","North Korea","South Korea","People's Republic of Yemen"))

#Calculating earliest democracy mentions
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, earliest.dem = min(YEAR[ has.mention ]))
democracy.mentions$earliest.dem <- ifelse(is.infinite(democracy.mentions$earliest.dem),NA,democracy.mentions$earliest.dem)
democracy.mentions[ !is.na(democracy.mentions$earliest.dem) & democracy.mentions$earliest.dem == min(democracy.mentions$earliest.dem,na.rm=TRUE) & democracy.mentions$YEAR == min(democracy.mentions$earliest.dem,na.rm=TRUE), ]
democracy.mentions$COUNTRY[ !is.na(democracy.mentions$earliest.dem) & democracy.mentions$earliest.dem == min(democracy.mentions$earliest.dem,na.rm=TRUE) & democracy.mentions$YEAR == min(democracy.mentions$earliest.dem,na.rm=TRUE)]

#Calculating last democracy mentions
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, latest.dem = max(YEAR[ has.mention ]))
democracy.mentions$latest.dem <- ifelse(is.infinite(democracy.mentions$latest.dem),NA,democracy.mentions$latest.dem)

#Last mention of democracy not in amendment or reinstatement
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, latest.dem.not.amendment = max(YEAR[ has.mention & EVENTTYPE %in% c("new","interim","suspension") ]))
democracy.mentions$latest.dem.not.amendment <- ifelse(is.infinite(democracy.mentions$latest.dem.not.amendment),NA,democracy.mentions$latest.dem.not.amendment)

#First and last constitutions in force
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, last.constitution.in.force = max(YEAR[ EVENTTYPE %in% c("new","interim","suspension") ]))
democracy.mentions$last.constitution.in.force <- ifelse(is.infinite(democracy.mentions$last.constitution.in.force),NA,democracy.mentions$last.constitution.in.force)
unique(democracy.mentions[ is.na(democracy.mentions$last.constitution.in.force), c("Name","COUNTRY","EVENTTYPE")]) #Six states do not have original texts, only amendments - Singapore, Brunei, Micronesia, Malaysia, Tonga, the Yemen People's Republic, and Scotland - so there's no "last constitution in force"
# We fix those with a regex

names <- as.character(democracy.mentions$Name[ democracy.mentions$COUNTRY %in% c("Singapore","Brunei","Micronesia, Fed. Sts.","Tonga","Yemen, People'S Republic Of", "Malaysia") ])

democracy.mentions$last.constitution.in.force[ democracy.mentions$COUNTRY %in% c("Singapore","Brunei","Micronesia, Fed. Sts.","Tonga","Yemen, People'S Republic Of", "Malaysia") ] <- as.numeric(regmatches(names,regexpr("(?<=Constitution of )[0-9]{4}",names,perl=TRUE)))
unique(democracy.mentions[ is.na(democracy.mentions$last.constitution.in.force), c("Name","COUNTRY") ])

democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, first.constitution.in.force = min(YEAR[ !(EVENTTYPE %in% c("draft","international convention"))]))
democracy.mentions$first.constitution.in.force <- ifelse(is.infinite(democracy.mentions$first.constitution.in.force),NA,democracy.mentions$first.constitution.in.force)

democracy.mentions$first.constitution.in.force[ democracy.mentions$last.constitution.in.force < democracy.mentions$first.constitution.in.force & !is.na(democracy.mentions$last.constitution.in.force)] <- democracy.mentions$last.constitution.in.force[ democracy.mentions$last.constitution.in.force < democracy.mentions$first.constitution.in.force & !is.na(democracy.mentions$last.constitution.in.force)]

#Countries that never mention democracy 
(never.mentions.dem <- unique(democracy.mentions[ is.na(democracy.mentions$latest.dem) & !(democracy.mentions$EVENTTYPE %in% "international convention"), c("COUNTRY") ])) #29 countries, some of which do not exist anymore
(never.mentions.dem <- paste(never.mentions.dem,collapse="|"))
#Countries which mentioned democracy at some point but do not mention it now
unique(democracy.mentions[ (democracy.mentions$latest.dem < democracy.mentions$last.constitution.in.force) & !is.na(democracy.mentions$latest.dem), c("COUNTRY","latest.dem","last.constitution.in.force") ]) # No country does this!

democracy.mentions$mentions.only.in.amendment <- FALSE
democracy.mentions[ democracy.mentions$has.mention, ] <- ddply(democracy.mentions[ democracy.mentions$has.mention, ], .(COUNTRY), transform, mentions.only.in.amendment = all(EVENTTYPE %in% "amendment"))
unique(democracy.mentions[ democracy.mentions$mentions.only.in.amendment, c("COUNTRY","mentions")])

# Countries where the first constitution is not the one that mentioned democracy
summary(na.omit(unique(democracy.mentions[ (democracy.mentions$earliest.dem > democracy.mentions$first.constitution.in.force) & !is.na(democracy.mentions$earliest.dem), c("COUNTRY","earliest.dem","first.constitution.in.force") ]))) # 78 countries, mean constitution year 1941

summary(na.omit(unique(democracy.mentions[ !is.na(democracy.mentions$earliest.dem) & (democracy.mentions$earliest.dem == democracy.mentions$first.constitution.in.force), c("COUNTRY","earliest.dem","first.constitution.in.force") ]))) # 108 countries, mean constitution year 1970 

#Countries with gaps

democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, has.gaps = !all(year.has.mention[EVENTTYPE %in% c("new","interim","suspension","reinstated") & YEAR > earliest.dem & YEAR < latest.dem]))
democracy.mentions <- ddply(democracy.mentions, .(COUNTRY), transform, gap.years = YEAR %in% YEAR[has.gaps & !year.has.mention & YEAR > earliest.dem & YEAR < latest.dem])  
unique(democracy.mentions[ democracy.mentions$has.gaps, c("COUNTRY")]) #42 countries


nrow(unique(democracy.mentions[ democracy.mentions$has.gaps & democracy.mentions$gap.years & democracy.mentions$EVENTTYPE %in% c("new","interim", "suspension","reinstated"), c("COUNTRY","YEAR","EVENTTYPE","gap.years", "year.has.mention","earliest.dem", "latest.dem")])) #73 "gap constitutions" that do not mention democracy even though a previous one does and a later one does
(unique(democracy.mentions[ democracy.mentions$has.gaps & democracy.mentions$gap.years & democracy.mentions$EVENTTYPE %in% c("new","interim","suspension","reinstated"), c("COUNTRY","YEAR","EVENTTYPE","gap.years", "year.has.mention","earliest.dem", "latest.dem")]))


write.csv(democracy.mentions,"democracy.mentions.csv",row.names=FALSE)


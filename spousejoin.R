#join the spouse file to the case file
spouse<-read.csv("CleanedSets4_10/Final_Spouse_Variables.csv")
SF_Case<-read.csv("SalesForce_Case.csv")

#How many of the spouses had cases (completed or otherwise)
spouseIDs<-spouse$Id
CaseIDs<-SF_Case$ContactId
sum(unique(spouseIDs) %in% unique(CaseIDs))

#Find client IDs that used federal resume review
caseFRR<-SF_Case %>% filter(Reason=="Federal Resume Review",Status=="Completed")
FRRIds <- caseFRR %>% select(ContactId) %>% mutate(count=1)

#Find client IDs that used Volunteer Services
usedVS<-volunteer_case_completed%>%select(ContactId)%>%mutate(count=1)
triedtouseVS<-volunteer_case_incomplete%>%select(ContactId)%>%mutate(count=1)

#group by and collapse
countsFRR<-group_by(as.tibble(FRRIds),ContactId)%>%summarise(FedResumeReview=sum(count))
countsUsedVS<-group_by(as.tibble(usedVS),ContactId)%>%summarise(VolunteerServicesUsed=sum(count))
countsUnusedVS<-group_by(as.tibble(triedtouseVS),ContactId)%>%summarise(VolunteerServicesIncomplete=sum(count))

#rightjoin to spouse df
spouseSQL<-merge(x = spouse, y = countsFRR, by.x = "Id",by.y="ContactId",all.x = TRUE)
spouseSQL<-merge(x=spouseSQL, y=countsUsedVS, by.x="Id",by.y="ContactId",all.x=TRUE)
spouseSQL<-merge(x=spouseSQL, y=countsUnusedVS, by.x="Id",by.y="ContactId",all.x=TRUE)

#write to csv
write.csv(spouseSQL,"spouseCaseJoin.csv")

getwd()
setwd("../MC2")
#newdata = read.csv(file = "loyaty_cc_join.csv", header = TRUE,na.strings ="")

#write.csv(data,file = "mydata.csv",row.names = F)
















#gps-loyalty
gps_orig=read_csv("gps.csv")
gps_d=gps_hm[c("location","id")]#,"location","id"
gps_d$timestamp=gps_orig$Timestamp
gps_d$timestamp=date_time_parse(gps_d$timestamp,
                                zone = "",
                                format="%m/%d/%Y")
gps_d=gps_d %>%
  distinct(timestamp,location,id, .keep_all = T)
loyalty_data2=loyalty_data %>%
  distinct(timestamp,location,loyaltynum, .keep_all = T)
joined_data_gpsd=merge(loyalty_data2,gps_d,all=FALSE)
joined_data_gpsd_group=joined_data_gpsd[c("loyaltynum","id")]
joined_data_gpsd_group=count.duplicates(joined_data_gpsd_group)

#gps-cc
gps_hm2=gps_hm %>%
  distinct(timestamp,location,id, .keep_all = T)
CC_data2=cc_data_origin %>%
  distinct(timestamp,location,last4ccnum, .keep_all = T)
joined_data_gpshm=merge(cc_data2,gps_hm2,all=FALSE)
joined_data_gpshm_group=joined_data_gpshm[c("last4ccnum","id")]
joined_data_gpshm_group=count.duplicates(joined_data_gpshm_group)

count.duplicates <- function(DF){
  x <- do.call('paste', c(DF, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
}


#clean-match
#temp=joined_data_gpshm_group
joined_data_gpshm_group=temp
#clean by unique last4
for(n in 1:112){
  #last column unique
  q=length(which(joined_data_gpshm_group$last4ccnum==joined_data_gpshm_group$last4ccnum[n]))==1
  if(q){
    #len_temp:how many match data in id
    len_temp=length(which(joined_data_gpshm_group$id==joined_data_gpshm_group$id[n]))
    for(i in 1:len_temp){
      #j:list-id locations
      #i:1,2,3,length_temp
      j=which(joined_data_gpshm_group$id==joined_data_gpshm_group$id[n])
      if(n!=j[i]){#except the original row
        #j[i]-location
        len_temp2=length(which(joined_data_gpshm_group$last4ccnum==joined_data_gpshm_group$last4ccnum[j[i]]))
        #len_temp2 if id match unique last4:1-unique >1 not unique
        if(len_temp2>1){#make sure the unmatch data not unique in last4
          #w=which(joined_data_gpshm_group$id==joined_data_gpshm_group$id[n])[i]
          
          print(paste("n ",n,joined_data_gpshm_group$last4ccnum[n]))
          print(paste("last4 id ",joined_data_gpshm_group$last4ccnum[j[i]],joined_data_gpshm_group$id[j[i]]))
          print(paste("j[i] ",j[i]))
          joined_data_gpshm_group$last4ccnum[j[i]]="0"
        }
      }
    }
  }
}
#delete
joined_data_gpshm_group=joined_data_gpshm_group %>%
  filter(joined_data_gpshm_group$last4ccnum!="0")
#clean by unique id
for(n in 1:81){
  #id column unique
  q=length(which(joined_data_gpshm_group$id==joined_data_gpshm_group$id[n]))==1
  if(q){
    #len_temp:how many match data in last4
    len_temp=length(which(joined_data_gpshm_group$last4ccnum==joined_data_gpshm_group$last4ccnum[n]))
    for(i in 1:len_temp){
      #j:list-last4 locations
      #i:1,2,3,length_temp
      j=which(joined_data_gpshm_group$last4ccnum==joined_data_gpshm_group$last4ccnum[n])
      if(n!=j[i]){#except the original row
        #j[i]-location
        len_temp2=length(which(joined_data_gpshm_group$id==joined_data_gpshm_group$id[j[i]]))
        #len_temp2 if id match unique id:1-unique >1 not unique
        if(len_temp2>1){#make sure the unmatch data not unique in id
          
          print(paste("n ",n,joined_data_gpshm_group$last4ccnum[n]))
          print(paste("last4 id ",joined_data_gpshm_group$last4ccnum[j[i]],joined_data_gpshm_group$id[j[i]]))
          print(paste("j[i] ",j[i]))
          joined_data_gpshm_group$count[j[i]]=0
        }
      }
    }
  }
}
joined_data_gpshm_group=joined_data_gpshm_group %>%
  filter(joined_data_gpshm_group$count!=0)
#link 3 data
names(joined_data_gpshm_group)[names(joined_data_gpshm_group) == 'last4ccnum'] = 'card_num'
relation1=merge(joined_data_gpshm_group,dictionary,all=TRUE)
names(relation1)[names(relation1) == 'count'] = 'count_cc'
relation1=merge(joined_data_gpshm_group,dictionary,all=TRUE)
relation2=joined_data_gpsd_group
names(relation2)[names(relation2) == 'loyaltynum'] = 'loyalty_num'
relation2=merge(relation1,relation2,all.x=TRUE)
names(relation2)[names(relation2) == 'count'] = 'count_la'
relation2$count=relation2$count_cc+relation2$count_la
relation2=relation2[c("id","loyalty_num","card_num","count")]

#network
car_assignm=read_csv("car-assignments.csv")
car_assignm$CarID=as_factor(car_assignm$CarID)
car_assignm=tidyr::unite(car_assignm, "Fullname", LastName, FirstName,sep = " ", remove = TRUE)
#rename
names(car_assignm)[names(car_assignm) == 'CarID'] = 'id'
car_assignm=car_assignm[1:3]
glimpse(car_assignm)
glimpse(joined_data_gpshm_group)
GAStech_graph=tbl_graph(nodes=car_assignm,
                        edges=joined_data_gpshm_group,
                        directed=TRUE)

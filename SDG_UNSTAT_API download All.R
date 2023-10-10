
library(httr)
library(jsonlite)
#library(openxlsx)
library(plyr)
library(openxlsx)
library(tidyr)
library(dplyr)
library(plotly)
library(maps)

# function to transform the SDG API data call into dataframe
api.to.json<- function (x) {
  x1<-content(x,"text")
  x2<-fromJSON(x1,flatten=TRUE)
  #x3<-as.data.frame(do.call("rbind", x2),stringsAsFactors=FALSE)
  return((x2))
}

#function to string together API call from SDG dataseries (y)
stringSDGseries<- function(y){
  seriescodes<-"https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?"
  for (x in y) {
    seriescodes<-paste(seriescodes,"seriesCode=",x,"&",sep = "")}
  return(seriescodes)
}

#function to string together API call from SDG dataseries (y) and return data for countrycode

SDGCountry<- function(y, countrycode){
  seriescodes<-"https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?"
  for (x in y) {
    seriescodes<-paste(seriescodes,"seriesCode=",x,"&",sep = "")}
  address<-paste(seriescodes,"areaCode=",countrycode,"&timePeriod=2000&timePeriod=2001&timePeriod=2002&timePeriod=2003&timePeriod=2004&timePeriod=2005&timePeriod=2006&timePeriod=2007&timePeriod=2008&timePeriod=2009&timePeriod=2010&timePeriod=2011&timePeriod=2012&timePeriod=2013&timePeriod=2014&timePeriod=2015&timePeriod=2016&timePeriod=2017&timePeriod=2018&timePeriod=2019&timePeriod=2020&timePeriod=2021&timePeriod=2022&timePeriod=2023&pageSize=15000",sep="")
  return(GET(address))
}



ALLsdgSeries<-GET("https://unstats.un.org/SDGAPI/v1/sdg/Series/List?allreleases=true")
json_ALLsdgseries<- unnest(as.data.frame(api.to.json(ALLsdgSeries)),cols = c(goal, target, indicator))


countrySDGdata<- function(countrycode)
{
  SDGgoals<- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17")  
  onecountrysdgdata<- data.frame()
  z<- data.frame()
  for (x in SDGgoals) 
  {
    goalseries<-json_ALLsdgseries[json_ALLsdgseries$goal==x,]
    goalseries<- unique(goalseries$code)
    
    if (length(goalseries)>50) 
    {
      v<- length(goalseries)
      v1<-floor(v/2)
      #print(v)
      onecountrysdgdata1<-SDGCountry(goalseries[1:v1],countrycode)
      #print(onecountrysdgdata1)
      onecountrysdgdata1<-api.to.json(onecountrysdgdata1)
      onecountrysdgdata1<- as.data.frame(onecountrysdgdata1[[7]])
      gseries_temp<-goalseries[v1+1:v]
      gseries_temp<-gseries_temp[!is.na(gseries_temp)]
      onecountrysdgdata2<-SDGCountry(gseries_temp,countrycode)
      onecountrysdgdata2<-api.to.json(onecountrysdgdata2)
      onecountrysdgdata2<- as.data.frame(onecountrysdgdata2[[7]])
      onecountrysdgdata<- rbind.fill(onecountrysdgdata1,onecountrysdgdata2)  
    } else 
    {
      onecountrysdgdata<-SDGCountry(goalseries,countrycode)
      onecountrysdgdata<-api.to.json(onecountrysdgdata)
      onecountrysdgdata<- as.data.frame(onecountrysdgdata[[7]])
    }
    
    z<- rbind.fill(z,onecountrysdgdata)
  }  
  
  return(z)
}

#all the countries
geoareacodes<- api.to.json(GET("https://unstats.un.org/SDGAPI/v1/sdg/GeoArea/List"))

esa<-geoareacodes[geoareacodes$geoAreaName %in% c("Angola"
                                                  ,"Botswana"
                                                  ,"Burundi"
                                                  ,"Comoros"
                                                  ,"Eritrea"
                                                  ,"Eswatini"
                                                  ,"Ethiopia"
                                                  ,"Kenya"
                                                  ,"Lesotho"
                                                  ,"Madagascar"
                                                  ,"Malawi"
                                                  ,"Mozambique"
                                                  ,"Namibia"
                                                  ,"Rwanda"
                                                  ,"Somalia"
                                                  ,"South Africa"
                                                  ,"South Sudan"
                                                  ,"United Republic of Tanzania"
                                                  ,"Uganda"
                                                  ,"Zambia"
                                                  ,"Zimbabwe"),1]

#mena<-c(12,262,275,364,368,400,422,504,788,818,887,634,48,512,784,414,729,434,682,760)


esadb=data.frame()
esalist<- list()
options(java.parameters = "-Xmx16000m")
ctr=0
for (x in esa) {
  ctr=ctr+1
  k<-countrySDGdata(x)
  
  listname<-unique(k$geoAreaName)
  esalist[[listname]]<-as.data.frame(k)
  esadb<-rbind.fill(esadb,k)
  print(paste(ctr,".Completed downloading country ",x))
}
backup_esadb<-esadb
save(esadb,file=paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\SDG Data ESA countries - ",Sys.Date(),".Rda", sep=""))
esadb$numeric_value<-as.numeric(esadb$value)


# Check for NA values in the esadb object
if (any(is.na(esadb))) {
  # Replace NA values with a suitable placeholder
  esadb[is.na(esadb)] <- "NA"
}

unlist_columns <- function(df) {
  for (col_name in names(df)) {
    if (is.list(df[[col_name]])) {
      df[[col_name]] <- unlist(df[[col_name]])
    }
  }
  return(df)
}

esadb<-unlist_columns(esadb)



esadb$data_ava_3<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-3), 1,0)
esadb$data_ava_5<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-5), 1,0)
esadb$data_ava_10<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-10), 1,0)
esadb$data_ava_15<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-15), 1,0)
esadb$data_ava_20<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-20), 1,0)
esadb$data_ava_m20<- if_else(!is.na(esadb$numeric_value) & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart< (as.integer(format(Sys.Date(),"%Y"))-20), 1,0)

esadb$data_ava_3yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-3), 1,0)
esadb$data_ava_5yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-5), 1,0)
esadb$data_ava_10yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-10), 1,0)
esadb$data_ava_15yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-15), 1,0)
esadb$data_ava_20yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart>= (as.integer(format(Sys.Date(),"%Y"))-20), 1,0)
esadb$data_ava_m20yC<- if_else(!is.na(esadb$numeric_value) & (esadb$attributes.Nature=="C" | esadb$attributes.Nature == "CA") & !is.na(esadb$timePeriodStart) & esadb$timePeriodStart< (as.integer(format(Sys.Date(),"%Y"))-20), 1,0)




#replace disaggregation values that can be dealt as separate variable by adding them in the indicator description
esadb$seriesDescription2<-if_else(esadb$`dimensions.Type of product`!="NA", paste(esadb$seriesDescription," - ", esadb$`dimensions.Type of product`),esadb$seriesDescription)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Education level`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Education level`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Name of non-communicable disease`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Name of non-communicable disease`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Type of occupation`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Type of occupation`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.IHR Capacity`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.IHR Capacity`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Name of international institution`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Name of international institution`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Type of speed`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Type of speed`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Type of skill`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Type of skill`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Grounds of discrimination`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Grounds of discrimination`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Cities`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Cities`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Food Waste Sector`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Food Waste Sector`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Mode of transportation`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Mode of transportation`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Policy Domains`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Policy Domains`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Counterpart`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Counterpart`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Frequency of Chlorophyll-a concentration`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Frequency of Chlorophyll-a concentration`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Sampling Stations`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Sampling Stations`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Land cover`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Land cover`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Bioclimatic belt`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Bioclimatic belt`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Parliamentary committees`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Parliamentary committees`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Fiscal intervention stage`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Fiscal intervention stage`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Level_of_government`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Level_of_government`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Level of requirement`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Level of requirement`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Population Group`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Population Group`),esadb$seriesDescription2)
esadb$seriesDescription2<-if_else(esadb$`dimensions.Policy instruments`!="NA", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Policy instruments`),esadb$seriesDescription2)


#add the adol birth rate by age
esadb$seriesDescription2<-if_else(esadb$series=="SP_DYN_ADKL", paste(esadb$seriesDescription2," - ", esadb$`dimensions.Age`),esadb$seriesDescription2)

#CRSDG match with SP goal areas
# Create a dataframe in R
crsdg_sp_map <- data.frame(
  sp_goal = c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 2, 2, 2),
  series = c('SE_DEV_ONTRK', 'SH_ACS_DTP3', 'SH_ACS_MCV2', 'SH_ACS_UNHC', 'SH_DYN_MORT', 'SH_DYN_NMRT', 'SH_HIV_INCD', 'SH_STA_BRTC', 'SH_STA_MORT', 'SH_STA_STNT', 'SH_STA_WAST', 'SN_STA_OVWGT', 'SP_DYN_ADKL', 'SE_PRE_PARTN', 'SE_TOT_PRFL', 'SG_REG_BRTH', 'SH_STA_FGMS', 'SL_TLF_CHLDEA', 'SL_TLF_CHLDEC', 'SP_DYN_MRBF15', 'SP_DYN_MRBF18', 'VC_IHR_PSRC', 'VC_VAW_MARR', 'VC_VAW_PHYPYV', 'VC_VAW_SXVLN', 'VC_VOV_SEXL', 'SE_ACC_HNDWSH', 'SE_ACS_H2O', 'SE_ACS_SANIT', 'SH_H2O_SAFE', 'SH_SAN_DEFECT', 'SH_SAN_HNDWSH', 'SH_SAN_SAFE', 'SP_ACS_BSRVH2O', 'SP_ACS_BSRVSAN', 'SD_MDP_CSMP', 'SD_MDP_MUHC', 'SI_COV_SOCAST', 'SI_POV_DAY1', 'SI_POV_NAHC','SE_ACS_ELECT',
             'SE_INF_DSBL', 'SE_TOT_CPLR')
)

esadb<-left_join(esadb,crsdg_sp_map,by=c('series'))

esadb$sp_goal[is.na(esadb$sp_goal)] <- "Non_SP"


esadb$CRSDG <- if_else(esadb$series %in% c("SD_MDP_CSMP",
                                           "SD_MDP_MUHC",
                                           "SE_ACC_HNDWSH",
                                           "SE_ACS_H2O",
                                           "SE_ACS_SANIT",
                                           "SE_DEV_ONTRK",
                                           "SE_PRE_PARTN",
                                           "SE_TOT_PRFL",
                                           "SG_REG_BRTH",
                                           "SH_ACS_DTP3",
                                           "SH_ACS_MCV2",
                                           "SH_ACS_UNHC",
                                           "SH_DYN_MORT",
                                           "SH_DYN_NMRT",
                                           "SH_H2O_SAFE",
                                           "SH_HIV_INCD",
                                           "SH_SAN_DEFECT",
                                           "SH_SAN_HNDWSH",
                                           "SH_SAN_SAFE",
                                           "SH_STA_BRTC",
                                           "SH_STA_FGMS",
                                           "SH_STA_MORT",
                                           "SH_STA_STNT",
                                           "SH_STA_WAST",
                                           "SI_COV_SOCAST",
                                           "SI_POV_DAY1",
                                           "SI_POV_NAHC",
                                           "SL_TLF_CHLDEA",
                                           "SL_TLF_CHLDEC",
                                           "SN_STA_OVWGT",
                                           "SP_ACS_BSRVH2O",
                                           "SP_ACS_BSRVSAN",
                                           "SP_DYN_ADKL",
                                           "SP_DYN_MRBF18",
                                           "SP_DYN_MRBF15",
                                           "VC_IHR_PSRC",
                                           "VC_VAW_MARR",
                                           "VC_VAW_PHYPYV",
                                           "VC_VAW_SXVLN",
                                           "VC_VOV_SEXL",
                                           "SE_ACS_ELECT",
                                           "SE_INF_DSBL",
                                           "SE_TOT_CPLR"
                                           ),
                       1,
                       0)

esadb<-dplyr::arrange(esadb,geoAreaName,goal,series, timePeriodStart)


#summarize by coutnry and indicator keeping latest date and value
esadb %>% group_by(geoAreaName,goal,target, indicator,series,seriesDescription2,dimensions.Location,dimensions.Sex, dimensions.Age,`dimensions.Education level`,`dimensions.Disability status`,dimensions.Activity,dimensions.Quantile,`dimensions.Migratory status`,`dimensions.Type of renewable technology`,`dimensions.Deviation Level`,`dimensions.Type of product`,`dimensions.Name of international institution`,`dimensions.Level/Status`) -> summaryesadb2
summaryesadb2 <- summaryesadb2 %>% dplyr::summarise(data3=sum(data_ava_3),data5=sum(data_ava_5),data10=sum(data_ava_10),data15=sum(data_ava_15),data20=sum(data_ava_20),datam20=sum(data_ava_m20),latestdate=max(timePeriodStart),latestvalue=value[which.max(timePeriodStart)],data3C=sum(data_ava_3yC),data5C=sum(data_ava_5yC),data10C=sum(data_ava_10yC),data15C=sum(data_ava_15yC),data20C=sum(data_ava_20yC),datam20C=sum(data_ava_m20yC))

disaggregates<- c('dimensions.Location','dimensions.Sex','dimensions.Age','dimensions.Disability status','dimensions.Quantile','dimensions.Activity','dimensions.Level/Status','dimensions.Deviation Level','dimensions.Type of renewable technology','dimensions.Migratory status')

summaryesadb2$crsdg <- if_else(summaryesadb2$series %in% c("SD_MDP_CSMP",
                                                           "SD_MDP_MUHC",
                                                           "SE_ACC_HNDWSH",
                                                           "SE_ACS_H2O",
                                                           "SE_ACS_SANIT",
                                                           "SE_DEV_ONTRK",
                                                           "SE_PRE_PARTN",
                                                           "SE_TOT_PRFL",
                                                           "SG_REG_BRTH",
                                                           "SH_ACS_DTP3",
                                                           "SH_ACS_MCV2",
                                                           "SH_ACS_UNHC",
                                                           "SH_DYN_MORT",
                                                           "SH_DYN_NMRT",
                                                           "SH_H2O_SAFE",
                                                           "SH_HIV_INCD",
                                                           "SH_SAN_DEFECT",
                                                           "SH_SAN_HNDWSH",
                                                           "SH_SAN_SAFE",
                                                           "SH_STA_BRTC",
                                                           "SH_STA_FGMS",
                                                           "SH_STA_MORT",
                                                           "SH_STA_STNT",
                                                           "SH_STA_WAST",
                                                           "SI_COV_SOCAST",
                                                           "SI_POV_DAY1",
                                                           "SI_POV_NAHC",
                                                           "SL_TLF_CHLDEA",
                                                           "SL_TLF_CHLDEC",
                                                           "SN_STA_OVWGT",
                                                           "SP_ACS_BSRVH2O",
                                                           "SP_ACS_BSRVSAN",
                                                           "SP_DYN_ADKL",
                                                           "SP_DYN_MRBF18",
                                                           "SP_DYN_MRBF15",
                                                           "VC_IHR_PSRC",
                                                           "VC_VAW_MARR",
                                                           "VC_VAW_PHYPYV",
                                                           "VC_VAW_SXVLN",
                                                           "VC_VOV_SEXL",
                                                           "SE_ACS_ELECT",
                                                           "SE_INF_DSBL",
                                                           "SE_TOT_CPLR"),
                               1,
                               0)

summaryesadb2<-left_join(summaryesadb2,crsdg_sp_map,by=c('series'))  
summaryesadb2$sp_goal[is.na(summaryesadb2$sp_goal)] <- "Non_SP"

totaldb <- data.frame()

# Initialize counters
cnt <- 0
cnt1 <- 0
cnt2 <- 0

# Get unique countries from geoAreaName column
countries <- unique(summaryesadb2$geoAreaName)

# Loop through each country
for (ctry in countries) {
  # Filter data for the current country
  cnt=cnt+1
  indicatorx <- summaryesadb2 %>% dplyr::filter(geoAreaName == ctry)
  
  # Get unique indicators for the current country
  indicators <- unique(indicatorx$seriesDescription2)
  
  # Loop through each indicator
  for (indicator in indicators) {
    # Filter data for the current indicator
    temp <- indicatorx[indicatorx$seriesDescription2 == indicator,]
    
    #check if indicator under more than 1 target
    if (length(unique(temp$indicator))>1) {temp<- temp %>% filter(indicator %in% unique(temp$indicator)[1])}
    
    temp[temp=="NA"]<-NA
    if (nrow(temp) == 1) {
      # If only one row, add it to totaldb
      # cnt <- cnt + 1
      # print(paste0('cnt+', cnt))
      totaldb <- rbind(totaldb, temp)
    } else if (nrow(temp) > 1) {
      # If multiple rows, filter based on disaggregates (!! madagascar upaid dom work dropped)
      #print(temp)
      for (dis in disaggregates) {
        if (sum(!is.na(temp[, dis])) > 1) {
          # cnt1 <- cnt1 + 1
          # print(paste0('cnt1+', cnt1))
          colname1<-paste0(dis)
          # solve the indicators of unpaid work with many age dis
          if (indicator %in% c("Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%)","Proportion of time spent on unpaid care work, by sex, age and location (%)","Proportion of time spent on unpaid domestic chores, by sex, age and location (%)")){ 
            temp<- temp %>%dplyr::filter(dimensions.Age %in% c('15+','10+','6-65'))}
          if (sum(!is.na(temp[, dis]))==2 && dis=="dimensions.Sex" && (sum(temp[,dis]=="FEMALE")==2)) next
          if (sum(!is.na(temp[, dis]))==3 && dis=="dimensions.Sex" && (sum(temp[,dis]=="FEMALE")==3)) next
          if (sum(!is.na(temp[, dis]))==2 && dis=="dimensions.Sex" && sum(temp[,dis]=="FEMALE")==1 && sum(temp[,dis]=="MALE")==1) { 
            temp <- temp %>% dplyr::filter(.data[[colname1]] %in% c('FEMALE'))} else {
              temp <- temp %>%
                dplyr::filter(.data[[colname1]] %in% c('ALLAREA', 'BOTHSEX', 'ALLAGE', '15-64','<5Y','_T', '15+','TOTAL','MEDIUM','ALL','30-70','M36T59'))
              # solve the anomaly of double age groups for int. poverty 
              if (sum(!is.na(temp[, dis]))==2 && dis=="dimensions.Age" && (sum(temp[,paste0("seriesDescription2")]==c("Proportion of population below international poverty line (%)"))==2)) {
                temp <- temp %>% dplyr::filter(.data[[colname1]] %in% c('ALLAGE'))}
            }
        }
      }
      totaldb <- rbind(totaldb, temp)
    } else {
      # cnt2 <- cnt2 + 1
      print(paste0('cnt2+', cnt2))
      next
    }
  }
  print(paste0(cnt," ",ctry, " is completed"))
}
save(totaldb,file=paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\SDG Data ESA countries - ",Sys.Date(),".Rda", sep=""))
backup_totaldb<-totaldb


totaldb$TotalDataAv<-totaldb$data20 + totaldb$datam20
totaldb$TotalDataAvC<- totaldb$data20C+ totaldb$datam20C




# Get the map data for the world
world_map <- map_data("world")

# Calculate the centroid for each country
centroids <- world_map %>%
  plotly::group_by(region) %>%
  dplyr::summarise(lon = mean(long), lat = mean(lat))

centroids$region[centroids$region=="Tanzania"] <- "United Republic of Tanzania"
centroids$region[centroids$region=="Swaziland"] <- "Eswatini"

# Merge centroids with summary_df by country name
totaldb <- totaldb %>%
  left_join(centroids, by = c("geoAreaName" = "region"))
totaldb<- as.data.frame(unlist_columns(totaldb))

write.xlsx(totaldb, paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\SDG Data Total ESA countries - ",Sys.Date(),".xlsx", sep=""))
write.xlsx(esadb,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\SDG Data ESA countries - ",Sys.Date(),".xlsx", sep=""))

write.csv(totaldb,"C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\data_availability2\\data\\totaldb.csv",row.names = FALSE)
write.csv(esadb,"C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\4. SDG\\2. SDG R API\\data_availability2\\data\\esadb.csv",row.names = FALSE)
write.csv(centroids,"data/centroids.csv",row.names = FALSE)


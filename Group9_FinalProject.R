library(readxl)
library(dplyr)
library(arules)
library(tidyr)
library(arulesViz)
library(colorspace)
library(dplyr)
library(stringr)
library(naniar)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(grid)
library(treemapify)
library(ggalt)
library(NbClust)
library(factoextra)
library(cluster)

df_orgin<-read_excel("Pakwheels_data.xlsx")
df_main<-df_orgin





###posted_data and added_via has some null values
### Main H-1:  How do features of Posted Cars vary according to City?
######## Sub-H-1): Which city have the most expensive cars posting?
######## Sub H-1): Are local car prefered in a specific city over the imported one?
######## Sub H-1): Which Manafacture is most popular in each of the city?
#######  Sub H-1): Does Clustering on features maps on the city features?
######   Sub H-1): Which type of Color is popular in each of the city?
#######  Sub H-1): Is there a difference in preference for totalmills across the different cities(New vs Really Old cars being sold)?
######   Sub H-1): Variation Across Consumption, Geartype and Body Type?


### Main H-2: Which feature of the car is the most important in determining the price?


### Main H-3: What kind of customer Segmentation is Possible?


### Intial data preprocessing
sapply(df_main,function(x)sum(is.na(x)))
unique(df_main$name)

### the following loops is needed in order to calculate maximum length of name which is a parameter needed in the separate function

names<-df_main$name
all_names <- list()
index=1
max_length<--100000
for (x in names){
  instance<-unlist(strsplit(x, '[[:space:]]'))
  all_names[index]<-instance
  current_length<-length(instance)
  if(current_length>=max_length){
    max_length<-current_length
  }
  index=index+1
}

### Extracting the Model_name from the orginal name column
df_main<-df_main %>% 
  separate(name, c("Manafacturer","I-1","I-2"),sep=" ")


df_main<-df_main %>% 
  unite(model_name, c("I-1", "I-2"))


### the following loops is needed in order to calculate maximum length of posted_location which is a parameter needed in the separate function

location_names<-df_main$posted_location
all_names <- list()
index=1
max_length<--100000
for (x in location_names){
  instance<-unlist(strsplit(x, '[[:space:]]'))
  all_names[index]<-instance
  current_length<-length(instance)
  if(current_length>=max_length){
    max_length<-current_length
  }
  index=index+1
}

### Extracting the Main Residential Location,City,Provinice from the orginal posted_location column
df_main<-df_main %>% 
  separate(posted_location, c("Main_Location","L_1"),sep=",")
df_main<-df_main %>% 
  separate(L_1, c("V_1","Posting_City","Province"),sep=" ")

df_main$V_1<-NULL
df_main$L_1<-NULL




df_main<-df_main %>% 
  mutate(Posting_City = coalesce(Posting_City,Main_Location))


df_main<-df_main %>% 
  mutate(Province = coalesce(Province,Main_Location))



df_main$Posting_City<-lapply(df_main$Posting_City,function(x){
  instance=unlist(strsplit(x, '[[:space:]]'))
  if (length(instance)>1){
    return (instance[1])}else{
      return(instance)
    }
})

df_main$Province<-lapply(df_main$Province,function(x){
  instance=unlist(strsplit(x, '[[:space:]]'))
  if (length(instance)>1){
    return (instance[2])
  }else{
    return(instance)
  }
})


#################

### Some user have not given their residential location therefore instead of repeating their city and province in this column, filling it with "Not Applicable"
temp<-df_main %>% 
  unite(temp, c("Posting_City", "Province"),sep = " ") %>% 
  select(temp)


temp<-c(unlist(temp[,1]))

temp<-unique(temp)
df_main<-df_main %>% 
  mutate(Main_Location=if_else(Main_Location %in% temp,"Not Applicable",Main_Location))


##################### The Above Preprocessing causes a slight problem;for cities wwhich are made of two or more words e.g Dera Ghazi Khan #########
### Since, there are few in number ,we can manually resolve this bug#####



## Identified cities with above problem
unique(df_main$Posting_City)#Kamra,Khan,Dina,Raiwind,Dera,Rahim,Hari,Bhai,Lower,Mian,Gujar,Kot,Fort,Mandi,Kallur
## Please mention in the report as to whow we detected this anomlaies and cleaned them.

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Khan", "Khan_qah_sharif"),
         Province=replace(Province, Province=="qah", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Dera", "Dera_Ismail_Khan"),
         Province=replace(Province, Province=="ismail", "KPK"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Rahim", "Rahim_Yar_Khan"),
         Province=replace(Province, Province=="Yar", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Hari", "Haripur"),
         Province=replace(Province, Province=="pur", "KPK"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Bhai", "Bhaipheru"),
         Province=replace(Province, Province=="pheru", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Lower", "Lower_Dir"),
         Province=replace(Province, Province=="Dir", "KPK"))



df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Mian", "Mian_Wali"),
         Province=replace(Province, Province=="Wali", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Gujar", "Gujar_Khan"),
         Province=replace(Province, Province=="Khan", "Punjab"))


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Kot", "Kot_radha_kishan"),
         Province=replace(Province, Province=="radha", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Fort", "Fort_Abbass"),
         Province=replace(Province, Province=="Abbass", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Mandi", "Mandibahauddin"),
         Province=replace(Province, Province=="bahauddin", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Kallur", "Kallur_Kot"),
         Province=replace(Province, Province=="kot", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Pindi", "Pindi_Bhattiya"),
         Province=replace(Province, Province=="Bhattiya", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Bahawal", "Bahawal_Nagar"),
         Province=replace(Province, Province=="Nagar", "Punjab"))


df_main<-df_main %>%
  mutate(Province=replace(Province, Province=="Azad", "A.K."))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Toba", "Toba_Tek_Singh"),
         Province=replace(Province, Province=="Tek", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Tando", "Tandoadam"),
         Province=replace(Province, Province=="adam", "Sindh"))


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Wah", "WahCantt"),
         Province=replace(Province, Province=="cantt", "Punjab"))

df_main$Province[df_main$Posting_City=="Nowshera"]="KPK"


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Fateh", "Fateh_Jang"),
         Province=replace(Province, Province=="Jang", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Shorekot", "ShorekotCantt"),
         Province=replace(Province, Province=="Cantt", "Punjab"))


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Tibba", "Tibbasultanpur"),
         Province=replace(Province, Province=="sultanpur", "Punjab"))

df_main<-df_main %>%
  mutate(Province=replace(Province, Province=="addu", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Nankana", "Nankanasahib"),
         Province=replace(Province, Province=="sahib", "Punjab"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Bat", "Bat_Khela"),
         Province=replace(Province, Province=="khela", "KPK"))

df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Kallar", "Kallar_Saddiyian"),
         Province=replace(Province, Province=="Saddiyian", "Punjab"))


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Renala", "Renalakhurd"),
         Province=replace(Province, Province=="khurd", "Punjab"))


df_main<-df_main %>%
  mutate(Posting_City=replace(Posting_City, Posting_City=="Mirpur", "MirpurKhas"),
         Province=replace(Province, Province=="khas", "Sindh"))

## checking whether problem has resolved
unique(df_main$Province)## 5 provinces  makes sense

########################################
## Identifying any null values as char or other anomalies for each column and filling with a good imputation strategy and/or converting the column to the desired type for subsequent analysis

###added_via 
## We know from the data scrapping that NULL values are actual posting from the desktop so we will fill the null values accordingly

df_main<-df_main %>% 
  mutate(added_via=if_else(added_via=="null","Added via Desktop",added_via))
df_main<-df_main %>% 
  mutate(added_via=replace_na(added_via,"Added via Desktop"))
View(df_main)

## Manafacturer


unique(df_main$Manafacturer)## No Anomaly identified

## model_name

unique(df_main$model_name)## No Anomaly identified



##totalmill
## it is character column but we need to convert it into numeric type for analysis purpose


df_main$totalmil<-lapply(df_main$totalmil, function(x){
  return (unlist(strsplit(x, split=' ', fixed=TRUE))[1])}) ## removing the km suffix e.g 8,000 km -- 8000


df_main$totalmil<-lapply(df_main$totalmil, function(x){str_replace_all(x, "[[:punct:]]", "")})## removing the ,  e.g 80,000 -- 80000
df_main$totalmil<-as.numeric(df_main$totalmil)

##consumption 
unique(df_main$consumption)## No Anomaly identified

###geartype
unique(df_main$geartype)

###registration_city
unique(df_main$registration_city)## No Anomaly identified

## Assembly
unique(df_main$Assembly)## No Anomaly identified

## bodytype
unique(df_main$bodytype)## Four Anomalies identified (Need to Replace Local,Imported,null,N/A)

View(df_main)

### body type will be imputation as per the model name so if there is a N/A or Null for City_1997, we can see what is the body type for this model in other rows and can fill it for our desired anomaly rows

### Imputation for N/A
temp<-df_main %>% filter(bodytype=="N/A" | bodytype=="null" | bodytype=="Local" | bodytype=="Imported") %>% 
  select(model_name)

temp<-unique(unlist(temp[,1]))

temp_2<-df_main %>% filter(model_name %in% temp) %>% 
  select(model_name,bodytype) %>% 
  distinct()



temp_3<-temp_2 %>% 
  separate(model_name, c("Model","Year"),sep="_") %>% 
  filter(!bodytype %in% c("null","Local","Imported","N/A")) %>% 
  select("Model","bodytype") %>% 
  distinct()### specific model and their bodytype, using these to fill the anomalies for other, if still not filled then will give a sperate category named "Not Specified"



unique_models_names_with_info<-unique(unlist(temp_3[,1]))

df_main<-df_main %>% 
  separate(model_name, c("Model","Year"),sep="_",remove = FALSE)## just doing it temporary, will remove them later


## imputating the value for body type
for (x in unique_models_names_with_info){
  bodtype_to_fill<-temp_3 %>% filter(Model==x) %>% select(bodytype)
  bodtype_to_fill<-c(unlist(bodtype_to_fill))[1]
  df_main<-df_main %>% mutate(bodytype=if_else(Model==x & bodytype %in% c("null","Local","Imported","N/A"),bodtype_to_fill,bodytype))
  }





## If there is still anomalies for body type this mean that it is not possible to imputate them given the data so we will make it seprate category called "Not Specify"
df_main<-df_main %>% mutate(bodytype=if_else(bodytype %in% c("null","Local","Imported","N/A"),"Not Specify",bodytype))

## dropping the temp columns
df_main$Model<-NULL
df_main$Year<-NULL

## imputating the value for color
unique(df_main$color) ## No Anomaly


#### imputating the value for engine_capacity
unique(df_main$engine_capacity) 
## there was three mistake in the dataset where user give 1cc,12cc,18cc which do make sense but we replaced these values by seeing the model name of each car
df_main$engine_capacity[df_main$engine_capacity=='1 cc']="1000cc"
df_main$engine_capacity[df_main$engine_capacity=='12 cc']="1200cc"
df_main$engine_capacity[df_main$engine_capacity=='18 cc']="1800cc"

#### imputating the value for posted_date
unique(df_main$posted_date) ## Many Anomalies and Almost the same date so can drop
df_main$posted_date<-NULL
#### No need for imputing Main_Location,Posting_City,Provinice since they are already preprocessed


## Converting price into numeric column
unique(df_main$price)
## there is price quotation in lacs,crore and there is call for price as well
## lacs and crore are easy to deal with 
## the call of price is been allocated Null for time being because we cannot assume the price beforehand

price_imputation_function<-function(x){
  main_list=unlist(strsplit(x, split=' ', fixed=TRUE))
  if (main_list[2]!="for"){numeric_value=as.numeric(main_list[2])}
  
  quotation=main_list[3]
  if (quotation=="lacs"){
    return (numeric_value*100000)
  }else if (quotation=="crore"){
    return (numeric_value*10000000)
  }else {return (-1)}
}

df_main$price<-unlist(lapply(df_main$price,price_imputation_function))

  
## Converting all of the desired columns into factors
df_main$Manafacturer<-as.factor(df_main$Manafacturer)
df_main$model_name<-as.factor(df_main$model_name)
df_main$consumption<-as.factor(df_main$consumption)
df_main$geartype<-as.factor(df_main$geartype)
df_main$registration_city<-as.factor(df_main$registration_city)
df_main$Assembly<-as.factor(df_main$Assembly)
df_main$color<-as.factor(df_main$color)
df_main$engine_capacity<-as.factor(df_main$engine_capacity)
df_main$Main_Location<-as.factor(df_main$Main_Location)
df_main$Posting_City<-as.factor(unlist(df_main$Posting_City))
df_main$Province<-as.factor(unlist(df_main$Province))
df_main$added_via<-as.factor(df_main$added_via)
df_main$bodytype<-as.factor(df_main$bodytype)
df_main$modelyear<-as.factor(df_main$modelyear)
## Reaffirming the numeric columns 
df_main$totalmil<-as.numeric(unlist(df_main$totalmil))
df_main$price<-as.numeric(unlist(df_main$price),na.rm=TRUE)



df_main<-df_main %>% 
  replace_with_na_at(.vars = c("price"),
                     condition = ~.x == -1)
######################################################################## Preprocessing Phase Done #####################################
### Start your analysis from here
df_train<-df_main
set.seed(12345)



###############################################################################################################
## Justification of the Hypothesis-EDA:
## Understanding how features for cars varies across different cities

### Counts of each posting city ## General EDA
theme_set(theme_classic())

city_freq<-df_train %>% 
  group_by(Posting_City) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

g <- ggplot(city_freq, aes(reorder(Posting_City,-count),count))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       xlab="City",
       subtitle="Posting City Counts", 
       caption="Source: Frequency of Cities from 'Pak Wheels' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Posting_City vs Manafacturer-->>Which Manafacture is most popular in each of the city?
manafacturer_city_count<-df_train %>% 
  select(Posting_City,Manafacturer) %>% 
  table() 
coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(manafacturer_city_count, Colv = NA, Rowv = NA, scale="column", col= coul, xlab="Manafacturer", ylab="City", main="Manufacturer and City Counts-HeadtMap",symm = FALSE)


## Posting_City vs Total_Mills
## Only taking into account the top5 most popular posting city 

df_temp<-df_main %>% 
  filter(Posting_City %in% c("Lahore","Karachi","Islamabad","Rawalpindi","Peshawar","Gujranwala","Faisalabad"))
g <- ggplot(df_temp, aes(totalmil))
g + geom_density(aes(fill=factor(Posting_City)), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Total Miles Grouped by Number of Posting City",
       caption="Source: Miles",
       x="Mileage",
       fill="# Cities")



g <- ggplot(df_temp, aes(Posting_City, totalmil))
g + geom_boxplot(aes(fill=factor(Posting_City))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="Total Miles Grouped by Number of Posting City(Top-7)",
       caption="Source: Miles",
       x="Posting Cities",
       y="Mileage")

### Consumption vs Cities

theme_set(theme_classic())
p1 <- ggplot(df_temp, aes(Posting_City))+ geom_bar(aes(fill=consumption), width = 0.5)+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
### Geartypes vs Cities
p2 <- ggplot(df_temp, aes(Posting_City)) + geom_bar(aes(fill=geartype), width = 0.5) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

### Cities vs Assembely
p3 <- ggplot(df_temp, aes(Posting_City))+ geom_bar(aes(fill=Assembly), width = 0.5)


### Combining all of the above plots for better comparsion
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),size = "last"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart",
       subtitle="Consumption,Geartypes and Assembely Variation across Cities (Top-7)",
       caption="Source: Posting_City from 'Pakwheel' dataset")


### Bodytype vs Cities

theme_set(theme_classic())
g <- ggplot(df_temp, aes(Posting_City))

g + geom_bar(aes(fill=bodytype), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Bodytype Variation Across Posting Cities", 
       caption="Source: Posting Cities from 'PakWheel' dataset")

### Color vs Cities
top_7_color<-df_temp %>% group_by(color) %>% summarise(count=n()) %>% arrange(desc(count))
color_temp_df<-df_temp %>% filter(color %in% c("White","Silver","Black","Grey","Blue","Red","Green"))
theme_set(theme_classic())
g <- ggplot(color_temp_df, aes(Posting_City))
g + geom_bar(aes(fill=color), width = 0.5,show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Color Variation Across Posting Cities", 
       caption="Source: Posting Cities from 'PakWheel' dataset")+
  geom_point(aes(y = 0, color = color), size = 0, shape = 95) +
  guides(fill = guide_legend(override.aes = list(size = 12)))

### Cities vs Engine Capcity
unique(df_temp$engine_capacity)
engine_capcity_temp<-df_temp
engine_capcity_temp$engine_capacity<-lapply(engine_capcity_temp$engine_capacity, function(x){str_replace_all(x, "cc", "")})
engine_capcity_temp$engine_capacity<-as.numeric(engine_capcity_temp$engine_capacity)
engine_capcity_temp$engine_capacity<-ordered(cut(engine_capcity_temp$engine_capacity, c(100, 1500, 3000, 4500,
                            + 5700)), labels = c("100-1500cc", "1500-3000cc", "3000-4500cc", "4500-6000cc"))
engine_capcity_temp$engine_capacity[is.na(engine_capcity_temp$engine_capacity)]="100-1500cc"
g <- ggplot(engine_capcity_temp, aes(Posting_City))+ geom_bar(aes(fill=engine_capacity), width = 0.5)
  g+theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Engine Capacity Variation Across Posting Cities", 
       caption="Source: Posting Cities from 'PakWheel' dataset")




# TreeMap for Main Residential Areas Frequencies Posting for Top-3 Cities
  
### Lahore
df_temp<-df_main %>% 
  filter(Posting_City %in% c("Lahore"))
treemap_coor<-df_temp %>% group_by(Posting_City,Main_Location) %>% 
  summarise(count=n())


ggplot(treemap_coor, aes(area = count, label = Posting_City,
                subgroup = Main_Location,fill=Main_Location)) +
  geom_treemap(aes(alpha=count)) +
  geom_treemap_subgroup_border(color="white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "bold", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+
  theme(legend.position = "none")

### Islamabad
df_temp<-df_main %>% 
  filter(Posting_City %in% c("Islamabad"))
treemap_coor<-df_temp %>% group_by(Posting_City,Main_Location) %>% 
  summarise(count=n())


ggplot(treemap_coor, aes(area = count, label = Posting_City,
                         subgroup = Main_Location,fill=Main_Location)) +
  geom_treemap(aes(alpha=count)) +
  geom_treemap_subgroup_border(color="white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "bold", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+
  theme(legend.position = "none")


### Karachi
df_temp<-df_main %>% 
  filter(Posting_City %in% c("Karachi"))
treemap_coor<-df_temp %>% group_by(Posting_City,Main_Location) %>% 
  summarise(count=n())


ggplot(treemap_coor, aes(area = count, label = Posting_City,
                         subgroup = Main_Location,fill=Main_Location)) +
  geom_treemap(aes(alpha=count)) +
  geom_treemap_subgroup_border(color="white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "bold", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+
  theme(legend.position = "none")

### Combining KHI,ISL,LHR
df_temp<-df_main %>% 
  filter(Posting_City %in% c("Karachi","Lahore","Islamabad"))
treemap_coor<-df_temp %>% group_by(Posting_City,Main_Location) %>% 
  summarise(count=n())


ggplot(treemap_coor, aes(area = count, label = Posting_City,
                         subgroup = Main_Location,fill=Main_Location)) +
  geom_treemap(aes(alpha=count)) +
  geom_treemap_subgroup_border(color="white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "bold", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+
  theme(legend.position = "none")



### Price across cities
df_temp<-df_main %>% 
  filter(Posting_City %in% c("Lahore","Karachi","Islamabad","Rawalpindi","Peshawar","Gujranwala","Faisalabad"))
g <- ggplot(df_temp, aes(Posting_City, price))
g + geom_boxplot(aes(fill=factor(Posting_City))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="Prices Grouped by Number of Posting City(Top-7)",
       caption="Source: Miles",
       x="Posting Cities",
       y="Price")
############### EDA for H1 Done ##################################################################

## model_year---need to included it

#### Now starting with H-1: Does Clustering on features maps on the city?
## Strategy to Answer: Going to use K_Means and compare


run_k_means<-function(k,df){
  ## This function runs kmeans for any k and returns the clusters
  
df$model_name<-NULL ## Provides the same information as column-1(Manafacturer)
df$Main_Location<-NULL ### No Location Information as per the hypothesis
df$Posting_City<-NULL ### No Location Information as per the hypothesis
df$Province<-NULL ### No Location Information as per the hypothesis
df$registration_city<-NULL ### No Location Information as per the hypothesis

df$Manafacturer<-as.integer(df$Manafacturer)
df$totalmil<-as.integer(df$totalmil)
df$consumption<-as.integer(df$consumption)
df$geartype<-as.integer(df$geartype)
df$Assembly<-as.integer(df$Assembly)
df$bodytype<-as.integer(df$bodytype)
df$color<-as.integer(df$color)
df$modelyear<-as.integer(df$modelyear)
df$engine_capacity<-as.integer(df$engine_capacity)
df$added_via<-as.integer(df$added_via)
df$price<-as.integer(df$price)

df$price[is.na(df$price)]<-0
df$price<-scale(df$price)
df$totalmil<-scale(df$totalmil)

kout <- kmeans(df,centers=k,nstart=10)## Runs the Kmean


return (kout$cluster)}


mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

####First  Try with k=3 since we have 3 main cities "Lahore","Karachi","Islamabad" and try to map the cluster features on the city features
df_train$cluster<-run_k_means(k=3,df_train)
df_temp<-df_train %>% 
  filter(Posting_City %in% c("Karachi","Lahore","Islamabad"))

## Features as per the Cluster  and Top-3 City

three_Cluster_features<-df_temp %>% 
  group_by(cluster) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),color=mode(color),engine_capacity=mode(engine_capacity),city=mode(Posting_City))

Top_3_City_features<-df_temp %>% 
  group_by(Posting_City) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),color=mode(color),engine_capacity=mode(engine_capacity))


View(three_Cluster_features)
View(Top_3_City_features)


##################################################################################################################################################
#### Now Try with k=7 since we have 7 main cities "Lahore","Karachi","Islamabad","Rawalpindi","Peshawar","Gujranwala","Faisalabad" and try to map the cluster features on the city features


df_train$cluster<-run_k_means(k=7,df_train)
df_temp<-df_train %>% 
  filter(Posting_City %in% c("Lahore","Karachi","Islamabad","Rawalpindi","Peshawar","Gujranwala","Faisalabad"))

## Features as per the Cluster  and Top-3 City

seven_Cluster_features<-df_temp %>% 
  group_by(cluster) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),color=mode(color),engine_capacity=mode(engine_capacity),city=mode(Posting_City))

Top_7_City_features<-df_temp %>% 
  group_by(Posting_City) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),color=mode(color),engine_capacity=mode(engine_capacity))


View(seven_Cluster_features)
View(Top_7_City_features)



## These  sub H-1s are also answered by the graphs
######   Sub H-1): Which type of Color is popular in each of the city?
#######  Sub H-1): Is there a difference in preference for totalmills across the different cities(New vs Really Old cars being sold)?
######   Sub H-1): Variation Across Consumption, Geartype and Body Type?

#####################################################H1:Does Clustering on features maps on the city? Done ##################################################################################### Hypothesis Analysis Finished





### Now starting with Main H-3: What kind of customer Segmentation do pak wheels have given the posting is Possible?
## Justification of the Hypothesis-EDA:
## What kind of customer Segmentation do pak wheels have given the posting is Possible?

options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.
View(df_train)
df_temp<-df_train %>% 
  filter(Province %in% c("Punjab","KPK","Balochistan","Sindh","Islamabad"))

df_temp$log_price<-log10(df_temp$price)
df_temp$log_total_mill<-log10(df_temp$totalmil)

df_temp_select_1<-df_temp %>% 
  filter(log_total_mill>=4,log_total_mill<6,log_price<7,log_price>5.1) ### Possible S-1

df_temp_select_2<-df_temp %>% 
  filter(log_total_mill>=0,log_total_mill<=3,log_price<7,log_price>5.1) ## Possible S-2

df_temp_select_3<-df_temp %>% 
  filter(log_total_mill>=4,log_total_mill<6,log_price>7,log_price<8) ## Possible S-3

View(df_temp)
# Jitterplot Total Mill Vs Price Grouped by Province and Possible Segments Encircle as per the features ##Shows some possible segmentation which means clustering on features is possible
gg <- ggplot(df_temp, aes(x=log_total_mill, y=log_price)) + 
  geom_jitter(width = .5, size=1,aes(shape=Province,col=Province)) + 
  geom_smooth(method="loess", se=F,color="black") +
  geom_encircle(aes(x=log_total_mill, y=log_price),
                data=df_temp_select_1,
                color="red",
                size=2,
                expand=0.08) +   # encircle
  geom_encircle(aes(x=log_total_mill, y=log_price),
                data=df_temp_select_2,
                color="blue",
                size=2,
                expand=0.08) +   # encircle
  geom_encircle(aes(x=log_total_mill, y=log_price),
                data=df_temp_select_3,
                color="orange",
                size=2,
                expand=0.08) + 
  xlim(c(1,6.5))+
  ylim(c(1,8))+# encircle
  labs(subtitle="Total Mill Vs Price Grouped by Province", 
       y="Log Price", 
       x="Log Total Mill", 
       title="Jitterplot + Encircle", 
       caption="Source: Pakwheel")

plot(gg)

############### EDA for H3 Done ##################################################################


## Customer Segmentation and what are those segments


K_means_customer_segmentation<-function(k,df){
  
  ## This is the same function as run_k_means but difference it includes geograhpical information as well because we are now segmenting customer base  in general
  
  df$model_name<-NULL ## Provides the same information as column-1(Manafacturer)
  df$Main_Location<-NULL ### Provincial Information is Enough
  df$Posting_City<-NULL  ### Provincial Information is Enough
  df$registration_city<-NULL  ### Provincial Information is Enough
  df_train$cluster<-NULL ## Removing Previous Cluster Info
  
  df$Manafacturer<-as.integer(df$Manafacturer)
  df$totalmil<-as.integer(df$totalmil)
  df$consumption<-as.integer(df$consumption)
  df$geartype<-as.integer(df$geartype)
  df$Assembly<-as.integer(df$Assembly)
  df$bodytype<-as.integer(df$bodytype)
  df$color<-as.integer(df$color)
  df$modelyear<-as.integer(df$modelyear)
  df$engine_capacity<-as.integer(df$engine_capacity)
  df$added_via<-as.integer(df$added_via)
  df$price<-as.integer(df$price)
  df$Province<-as.integer(df$Province) 

  
  
  df$price[is.na(df$price)]<-0
  df$price<-scale(df$price)
  df$totalmil<-scale(df$totalmil)
  
  kout <- kmeans(df,centers=k,nstart=10)## Runs the Kmean
  
  plot(fviz_cluster(kout,df, geom=c("point")))## Drawing the Cluster plot on two most important features
  return (kout)
}


## As per our EDA there might be 3 segments so starting with 3 segments
K_3_out<-K_means_customer_segmentation(k=3,df_train)

K_3_out
df_train$cluster<-K_3_out$cluster

## feture of 3 segments
three_Segment_features<-df_train %>% 
  group_by(cluster) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),added_via=mode(added_via),color=mode(color),engine_capacity=mode(engine_capacity))
View(three_Segment_features)



### 3 Segments are not enrich enough to differentiate therefore moving with higher K,jumping to 4
K_4_out<-K_means_customer_segmentation(k=4,df_train)
K_4_out
df_train$cluster<-K_4_out$cluster

## feture of 4 segments
four_Segment_features<-df_train %>% 
  group_by(cluster) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),added_via=mode(added_via),color=mode(color),engine_capacity=mode(engine_capacity))
View(four_Segment_features)





### 4 Segments are  enrich enough to differentiate but still moving with higher K just for testing purposes,jumping to 5
K_5_out<-K_means_customer_segmentation(k=5,df_train)
K_5_out
df_train$cluster<-K_5_out$cluster

## feture of 5 segments
five_Segment_features<-df_train %>% 
  group_by(cluster) %>% 
  summarise(Manafacturer=mode(Manafacturer),totalmil_mean=mean(totalmil),price_mean=mean(price,na.rm=TRUE),consumption=mode(consumption),geartype=mode(geartype),Assembly=mode(Assembly),bodytype=mode(bodytype),added_via=mode(added_via),color=mode(color),engine_capacity=mode(engine_capacity))
View(five_Segment_features)



### Five Segments are Good Enough
### First Cluster--> Posting for Cheap Local Cars
### Second,Third Cluster-->> Posting for  Expensive Imported Cars (Second on PRemium Price Point and third on relatively lower)
### Fourth,Fifth Cluster-->> Posting for  Mid Range Local Cars (Fourth on 1300cc and third on 1000cc)




### Trying in Build Index method to see what automatic selection suggest the number of k should be given the data



automatic_indexation_k<-function(df,index){
  
  ## This is the same function as run_k_means but difference it includes geograhpical information as well because we are now segmenting customer base  in general
  
  df$model_name<-NULL ## Provides the same information as column-1(Manafacturer)
  df$Main_Location<-NULL ### Provincial Information is Enough
  df$Posting_City<-NULL  ### Provincial Information is Enough
  df$registration_city<-NULL  ### Provincial Information is Enough
  df_train$cluster<-NULL ## Removing Previous Cluster Info
  
  df$Manafacturer<-as.integer(df$Manafacturer)
  df$totalmil<-as.integer(df$totalmil)
  df$consumption<-as.integer(df$consumption)
  df$geartype<-as.integer(df$geartype)
  df$Assembly<-as.integer(df$Assembly)
  df$bodytype<-as.integer(df$bodytype)
  df$color<-as.integer(df$color)
  df$engine_capacity<-as.integer(df$engine_capacity)
  df$added_via<-as.integer(df$added_via)
  df$price<-as.integer(df$price)
  df$Province<-as.integer(df$Province) 
  df$modelyear<-as.integer(df$modelyear)
  
  
  df$price[is.na(df$price)]<-0
  df$price<-scale(df$price)
  df$totalmil<-scale(df$totalmil)
  
  ## silhouette
  if(index=="silhouette" | index=="wss"){fviz_nbclust(df,FUN=kmeans,method=index)}
  else{
    nb<- NbClust(df, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")
    return (nb)
  }
   
  
}

## silhouette
automatic_indexation_k(df_train,"silhouette")

## wss
automatic_indexation_k(df_train,"wss")

## 
nb<-automatic_indexation_k(df_train,"all") ## 5 clusters are good enough as per the automatic selection
fviz_nbclust(nb)

#####################################################H3:What kind of customer Segmentation do pak wheels have given the posting is Possible? Done ##################################################################################### Hypothesis Analysis Finished



### Main H-2: Which features are the most important in determining the price?

##Data exploration with graphs for this hypothesis - looking at how price effects different variables

vizdata <- df_train
vizdata$cluster <- NULL
summary(vizdata)
colSums(is.na(vizdata))
vizdata <- na.omit(vizdata)

options(scipen=999)

#Price & Total Mileage Scatterplot
ggplot(vizdata, aes(price,totalmil))+
  geom_point()+
  scale_x_log10() +
  labs(x = "Price", y = "Total Mileage", title = "Price & Total Mileage Plot")

#Price and Manafacturer
ggplot(vizdata, aes(Manafacturer, price))+ 
  geom_col()+
  labs(x = "Manufacturer", y = "Price", title = "Price & Manufacturer Plot")

#Price and Assembly 
ggplot(vizdata, aes(Assembly, price))+
  geom_col() +
  labs(x = "Assembly of Cars", y = "Price", title = "Price & Assembly Plot")

#Price and Gear Type
ggplot(vizdata, aes(geartype, price))+
  geom_col()+
  theme_bw()+
  labs(x = "Gear Type", y = "Price", title = "Price & Gear Type Plot")

#Price and Engine Capacity
ggplot(vizdata, aes(engine_capacity,price))+
  geom_col()+
  theme_bw()+
  labs(x = "Engine Capacity", y = "Price", title = "Price & Engine Capacity Plot")

#Price and Registration City
ggplot(vizdata, aes(registration_city,price))+
  geom_col()+
  theme_bw()+
  labs(x = "Registration City", y = "Price", title = "Price & Registration City Plot")

vizdata01 <- vizdata %>%
  arrange(modelyear)

#Price and Model Year
ggplot(vizdata01, aes(modelyear, price))+
  geom_col()+
  theme_bw()+
  labs(x = "Model Year", y = "Price", title = "Price & Model-Year Plot")


##Data Prep for Arules
pakwheels <- df_train
pakwheels$cluster <- NULL


summary(pakwheels)
#price range is from 150,000 to 48,500,000 - mean is 2,642,422
#total mileage range is from 1 (min) to 950,000 - mean is 79,472

##Arules
pakwheels$price <- ordered(cut(pakwheels$price, c(0, 1100000, 2050000, 7000000, 15000000, 50000000)), labels = c("Very Cheap","Cheap","Moderate", "High", "Very High"))
pakwheels$totalmil <- ordered(cut(pakwheels$totalmil, c(0,50000, 100000, 960000)), labels = c("Low","Moderate", "High"))

pakwheels <- na.omit(pakwheels)

cars <- as(pakwheels,"transactions")
summary(cars)

inspect(cars[1:5])
inspect(cars[1:10])

#Not used:
itemFrequency(cars)
itemFrequency(cars[,1:10])

dev.off()
#Not used:
#itemFrequencyPlot(cars)
itemFrequencyPlot(cars, support = 0.5, main = "ItemFrequency Plot - Support = 0.1")
itemFrequencyPlot(cars, support = 0.2, main = "ItemFrequency Plot - Support = 0.15")
itemFrequencyPlot(cars, support = 0.1, main = "ItemFrequency Plot - Support = 0.08")

#Not used
itemFrequencyPlot(cars,topN = 10)
itemFrequencyPlot(cars,topN = 15)
itemFrequencyPlot(cars,topN = 20)

#General rules 
grules <- apriori(cars)
grules
plot(grules)
summary(grules)
inspect(grules[1:10])

grules1 <- apriori(cars, parameter = list(support = 0.2, conf = 0.5))
grules1
plot(grules1)
summary(grules1)
inspect(grules1[1:10])

grules2 <- apriori(cars, parameter = list(support = 0.5, conf = 0.5))
grules2
plot(grules2)
summary(grules2)
inspect(grules2[1:10])

grules3 <- apriori(cars, parameter = list(support = 0.3, conf = 0.25))
grules3
plot(grules3)
summary(grules3)
inspect(grules3[1:10])

grules4 <- apriori(cars, parameter = list(support = 0.1, conf = 0.5))
grules4
plot(grules4)
summary(grules4)
inspect(grules4[1:10])

grules5 <- apriori(cars, parameter = list(support = 0.1, conf = 0.8))
grules5
plot(grules5)
summary(grules5)
inspect(grules5[1:10])
grules5 <- sort(grules5, by = "lift")
inspect(grules5[1:10])

grules6 <- apriori(cars, parameter = list(support = 0.1, conf = 0.9))
grules6
plot(grules6)
summary(grules6)
inspect(grules6[1:10])
grules6 <- sort(grules6, by = "lift")
inspect(grules6[1:10])

grules7 <- apriori(cars, parameter = list(support = 0.15, conf = 0.9))
grules7
plot(grules7)
summary(grules7)
inspect(grules7[1:10])
grules7 <- sort(grules7, by = "lift")
inspect(grules7[1:10])

grules8 <- apriori(cars, parameter = list(support = 0.15, conf = 0.8))
grules8
plot(grules8)
summary(grules8)
inspect(grules8[1:10])
grules8 <- sort(grules8, by = "lift")
inspect(grules8[1:10])
#Choosing a larger value for confidence than the usual threshold value since from grules4 we can see that there are a lot of rules near 1 (further support can be found from the summary of grules)

grules9 <- apriori(cars, parameter = list(support = 0.15, conf = 0.9))
grules9
plot(grules9, verbose=TRUE) #scatter plot for rules
summary(grules9)
plot(grules9, method= "grouped")
plot(grules9, method="grouped", measure =c("confidence","lift")) 
inspect(grules9[1:10])
grules9 <- sort(grules9, by = "lift")
inspect(grules9[1:10])

grules10 <- apriori(cars, parameter = list(support=0.1, conf =1))
grules10
plot(grules10)
summary(grules10)

grules10 <- sort (grules10, by = "lift") #sort by lift
inspect(grules10[1:10]) # look at top 10 rules
plot(grules10, method= "grouped")
plot(grules10, method="grouped", measure =c("confidence","lift")) 

##Subsetting rules to more car specific features

car_rules1 <- subset(grules, subset = rhs %in% "bodytype=Hatchback" & lift > 1)
car_rules1
inspect(sort(car_rules1, by = "confidence")[1:10])

car_rules2 <- subset(grules, subset = rhs %in% "Assembly=Imported" & lift > 1)
car_rules2
inspect(sort(car_rules2, by = "confidence")[1:10])

car_rules3 <- subset(grules, subset = rhs %in% "geartype=Automatic" & lift > 1)
car_rules3
inspect(sort(car_rules3, by = "confidence")[1:10])

dev.off()

#Now fixing Price on the RHS - this is done on the different categories of price
#Tried not to lessen the support too much because if those rules are too uncommon, they are not of much value - want them to be representative
#Furthermore, if the support was too less, it was quite difficult to interpret the visualizations clearly

#creating rules for very cheaply priced cars
cars_vcheap <- apriori(cars, parameter = list (support = 0.05, conf = 0.7), appearance = list (rhs = "price=Very Cheap", default = "lhs")) #least support to see more meaningful rules
cars_vcheap  

cars_vcheap <- sort (cars_vcheap, by = "lift") #sort by lift
inspect(cars_vcheap[1:10]) #look at top 10 rules
plot(cars_vcheap, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_vcheap, verbose=TRUE) #scatter plot for rules

#creating rules for cheaply priced cars
cars_cheap <- apriori(cars, parameter = list (support = 0.05, conf = 0.7), appearance = list (rhs = "price=Cheap", default = "lhs")) #least support to see more meaningful rules
cars_cheap  

cars_cheap <- sort (cars_cheap, by = "lift") #sort by lift
inspect(cars_cheap[1:10]) #look at top 10 rules
plot(cars_cheap, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_cheap, verbose=TRUE) #scatter plot for rules

#creating rules for moderately priced cars
cars_moderate <- apriori(cars, parameter = list (support = 0.035, conf = 0.7), appearance = list (rhs = "price=Moderate", default = "lhs")) #least support to see more meaningful rules
cars_moderate 

cars_moderate <- sort (cars_moderate, by = "lift") #sort by lift
inspect(cars_moderate[1:10]) #look at top 10 rules
plot(cars_moderate, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_moderate, verbose=TRUE) #scatter plot for rules

#creating rules for highly priced cars
cars_high <- apriori(cars, parameter = list (support = 0.01, conf = 0.2), appearance = list (rhs = "price=High", default = "lhs")) #got 0 rules when increased support to even 0.03 and confidence to 0.5 so lowered values
cars_high

cars_high <- sort (cars_high, by = "lift") #sort by lift
inspect(cars_high[1:10]) #look at top 10 rules
plot(cars_high, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_high, verbose=TRUE) #scatter plot for rules

#creating rules for very highly priced cars
cars_vhigh <- apriori(cars, parameter = list (support = 0.01, conf = 0.5), appearance = list (rhs = "price=Very High", default = "lhs")) #least support to see more meaningful rules
cars_vhigh

cars_vhigh <- sort (cars_vhigh, by = "lift") #sort by lift
inspect(cars_vhigh[1:10]) #look at top 10 rules
plot(cars_vhigh, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_vhigh, verbose=TRUE) #scatter plot for rules


## Sub Hypothesis - Which feature of the car is the most important in determining the price? ####################################

#Only including features of cars

carfeatures <- df_train
carfeatures$Main_Location <- NULL
carfeatures$registration_city <- NULL 
carfeatures$Posting_City <- NULL
carfeatures$Province <- NULL
carfeatures$added_via <- NULL
carfeatures$cluster <- NULL

carfeatures$price <- ordered(cut(carfeatures$price, c(0, 1100000, 2050000, 7000000, 15000000, 50000000)), labels = c("Very Cheap","Cheap","Moderate", "High", "Very High"))
carfeatures$totalmil <- ordered(cut(carfeatures$totalmil, c(0,50000, 100000, 960000)), labels = c("Low","Moderate", "High"))

carfeatures <- na.omit(carfeatures)

cars1 <- as(carfeatures,"transactions")
summary(cars)

inspect(cars1[1:10])

#General rules 
grules <- apriori(cars1)
grules
plot(grules)
inspect(cars1[1:10])

#Now fixing Price on the RHS - this is done on the different categories of price
#Tried not to lessen the support too much because if those rules are too uncommon, they are not of much value - want them to be representative
#Furthermore, if the support was too less, it was quite difficult to interpret the visualizations clearly

#creating rules for very cheaply priced cars
cars_vcheap <- apriori(cars1, parameter = list (support = 0.05, conf = 0.7), appearance = list (rhs = "price=Very Cheap", default = "lhs")) #least support to see more meaningful rules
cars_vcheap  

cars_vcheap <- sort (cars_vcheap, by = "lift") #sort by lift
inspect(cars_vcheap[1:10]) #look at top 10 rules
plot(cars_vcheap, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_vcheap, verbose=TRUE) #scatter plot for rules

#creating rules for cheaply priced cars
cars_cheap <- apriori(cars1, parameter = list (support = 0.05, conf = 0.7), appearance = list (rhs = "price=Cheap", default = "lhs")) #least support to see more meaningful rules
cars_cheap  

cars_cheap <- sort (cars_cheap, by = "lift") #sort by lift
inspect(cars_cheap[1:10]) #look at top 10 rules
plot(cars_cheap, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_cheap, verbose=TRUE) #scatter plot for rules

#creating rules for moderately priced cars
cars_moderate <- apriori(cars1, parameter = list (support = 0.035, conf = 0.7), appearance = list (rhs = "price=Moderate", default = "lhs")) #least support to see more meaningful rules
cars_moderate 

cars_moderate <- sort (cars_moderate, by = "lift") #sort by lift
inspect(cars_moderate[1:10]) #look at top 10 rules
plot(cars_moderate, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_moderate, verbose=TRUE) #scatter plot for rules

#creating rules for highly priced cars
cars_high <- apriori(cars1, parameter = list (support = 0.01, conf = 0.2), appearance = list (rhs = "price=High", default = "lhs")) #got 0 rules when increased support to even 0.03 and confidence to 0.5 so lowered values
cars_high

cars_high <- sort (cars_high, by = "lift") #sort by lift
inspect(cars_high[1:10]) #look at top 10 rules
plot(cars_high, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_high, verbose=TRUE) #scatter plot for rules

#creating rules for very highly priced cars
cars_vhigh <- apriori(cars1, parameter = list (support = 0.01, conf = 0.5), appearance = list (rhs = "price=Very High", default = "lhs")) #least support to see more meaningful rules
cars_vhigh

cars_vhigh <- sort (cars_vhigh, by = "lift") #sort by lift
inspect(cars_vhigh[1:10]) #look at top 10 rules
plot(cars_vhigh, method="grouped", measure =c("confidence","lift")) #shows grouped matrix for rules

plot(cars_vhigh, verbose=TRUE) #scatter plot for rules


## Logistic Regression (for the main hypothesis) ####################################################################################

#dataprep
pkwheels <- df_train
pkwheels$cluster <- NULL
summary(pkwheels)
pkwheels$totalmil <- ordered(cut(pkwheels$totalmil, c(0,50000, 100000, 960000)), labels = c("Low","Moderate", "High"))
pkwheels$price <- ordered(cut(pkwheels$price, c(0, 2800000, 50000000)), labels = c("Low","High"))

#pkwheels <- na.omit(pkwheels)
pkwheels$price <- ifelse(pkwheels$price == "High", 1,0)

dt <- sort(sample(nrow(pkwheels),nrow(pkwheels)*0.7))

set.seed(12345)

train <- pkwheels[dt,]
test <- pkwheels[-dt,]

#Model 1
LR <- glm(price~consumption+bodytype+Assembly+geartype+totalmil,train,family="binomial")
summary(LR)
#hit & trial was conducted to get the best combination of independent variables 
#LR model was made 
#Single out significant p values - those less than the threshold of 0.05

pred_prob <- predict(LR,test)
View(pred_prob)

predicted_outcome <- rep("0",dim(test)[1])
View(predicted_outcome)

predicted_outcome[pred_prob>0.8] <- "1"

table(predicted_outcome)

#confusion matrix 
table(predicted_outcome,test$price)

#Model 2 - not used - no significant values
LR1 <- glm(price~.,train,family="binomial")
summary(LR1)

#Model 3 - only one significant value
LR3 <- glm(price~Posting_City,train,family="binomial")
summary(LR3)

#Model 4 - no significant values
LR4 <- glm(price~color,train,family="binomial")
summary(LR4)

#Model 5 - no significant values
LR5 <- glm(price~engine_capacity,train,family="binomial")
summary(LR5)

#Model 6 - only one significant value
LR6 <- glm(price~Posting_City,train, family="binomial")
summary(LR6)

#Model 1 yielded the best results and therefore, we will consider that














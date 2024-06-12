###############
#  PACKAGES   #
###############


if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(tidyverse)) {
    install.packages("tidyverse")  
}
if (!require(sf)) {
  install.packages("sf")  
}
if (!require(mapSpain)) {
  install.packages("mapSpain")  
}
if (!require(dunn.test)) {
  install.packages("dunn.test")  
}
if (!require(gmodels)) {
  install.packages("gmodels")  
}
if (!require(bibliometrix)) {
  install.packages("bibliometrix")  
}
if (!require(gridExtra)) {
  install.packages("gridExtra")  
}
if (!require(spatstat)) {
  install.packages("spatstat")  
}
if (!require(compareGroups)) {
  install.packages("compareGroups")
}
if (!require(MASS)) {
  install.packages("MASS")
}
if (!require(DescTools)) {
  install.packages("DescTools")
}
if (!require(brant)) {
  install.packages("brant")
}
if (!require(car)) {
  install.packages("car")
}
if (!require(Hmisc)) {
  install.packages("Hmisc")
}
if (!require(VGAM)) {
  install.packages("VGAM")
}

library(brant)
library(car)
library(dplyr)
library(tidyverse)
library(sf)
library(spatstat)
library(gmodels)
library(mapSpain)
library(dunn.test)
library(bibliometrix)
library(gridExtra)
library(compareGroups)
library(MASS)
library(DescTools)
library(Hmisc)
library(VGAM) 

###############
#   DATASET   #
###############

wd<-"/Users/albertmartinezpuig/Desktop/UOC/4t SEMESTRE/TFM/BBDD"
setwd(wd)

D<-read.table("rescatsbombers.txt",sep="\t",header=T,na.strings="NA")
municip <- read.csv("municip.csv")
elev <- read.csv("rescats_elevacio.csv",header=T,sep=";")
elev=elev[,c(1,4)]
names(elev)[2] = "Altitude"

D <- merge(D, elev, by = "ACT_NUM_ACTUACIO", all.x = TRUE)
D$Altitude[which(D$Altitude < 0)] = NA


Cat_shape<-st_read("/Users/albertmartinezpuig/Desktop/UOC/4t SEMESTRE/TFM/Mapes/Catalunya/CatalunyaPeninsular.shp")
Cat <- st_read("/Users/albertmartinezpuig/Desktop/UOC/4t SEMESTRE/TFM/Mapes/divisions-administratives-v2r1-20240118/divisions-administratives-v2r1-comarques-5000-20240118.shp")  
mun <- st_read("/Users/albertmartinezpuig/Desktop/UOC/4t SEMESTRE/TFM/Mapes/divisions-administratives-v2r1-20240118/divisions-administratives-v2r1-municipis-5000-20240118.shp")   
Nat<-st_read("/Users/albertmartinezpuig/Desktop/UOC/4t SEMESTRE/TFM/Mapes/ESPAISNATURALS_ENPE/ESPAISNATURALS_ENPE/ESPAISNATURALS_ENPE.shp")


###############
#  VARIABLES  #
###############

D$SEX=factor(D$SEX,labels = c("D" = "Female", "H" = "Male","M"="Mixed"))

D$LESIO <- factor(D$LESIO, levels = c("Res", "Lleu", "Z5", "Z6"))

D$GRUP<-factor(D$GRUP, levels = c("S", "G", "Cursa"))

D$ACT_NIV2_COD <-factor(D$ACT_NIV2_COD)
names <- c("excursionisme", "neu", "recoleccio", "escalada", "escalada en gel", "barranquisme", "ciclisme", "altres")
levels(D$ACT_NIV2_COD) <- names
#Ice-Climbing into snow categories because of scarce data
D$ACT_NIV2_COD[D$ACT_NIV2_COD=="escalada en gel"] = "neu"
D$ACT_NIV2_COD <- droplevels(D$ACT_NIV2_COD, exclude = "escalada en gel")
D$ACT_NIV2_COD <- factor(D$ACT_NIV2_COD, labels =  c("excursionisme" = "Hiking", "neu" = "Snow", 
                                                     "recoleccio" = "Gathering", "escalada" = "Climbing"
                                                     , "barranquisme" = "Canyoning", 
                                                     "ciclisme" = "Mountain Biking", "altres" = "Others"))

D$ACT_PERILLOSA <- factor(D$ACT_PERILLOSA, labels = c("N" = "No", "S" = "Yes"))

D$ACT_EQUIPAMENT_ADEQUAT<- factor(D$ACT_EQUIPAMENT_ADEQUAT, labels = c("N" = "No", "S" = "Yes"))

D$GRUP<-factor(D$GRUP,labels = c("S" = "Alone", "G" = "Group", "Cursa" = "Competition"))

D$LESIO <-factor(D$LESIO,labels = c("Res" = "Unharmed", "Lleu" = "Injured/Ill", 
                                    "Z5" = "Life Risk", "Z6" = "Death"))
D <- D %>%
  mutate(TYPE_INJURY = case_when(
    TIPUSLESIO %in% c("Luxacio", "Fractura", "Caiguda", "Esguinc", "Contusio", "Ferida", "Hemorragia","Muscular","TCE") ~ "Trauma",
    TIPUSLESIO %in% c("PCR", "Cardiac", "Ictus", "Infart") ~ "Cardiovascular",
    TIPUSLESIO %in% c("Neumotorax", "Ofegat", "Respiratori") ~ "Breath",
    TIPUSLESIO == "Mareig" ~ "Conscious/Heat",
    TIPUSLESIO %in% c("Altres", "Epilepsia", "Picadura", "Hipotermia") ~ "Others",
    TIPUSLESIO == "Ansietat" ~ "Anxiety",
    TIPUSLESIO %in% c("Desorientacio", "Tecnic", "Fatiga") ~ "Technical",
    TRUE ~ NA  # Cualquier otro caso
  ))

D$TYPE_INJURY=factor(D$TYPE_INJURY)
D$TRAUMA_RELATED <- D$TIPUSLESIO
D$TRAUMA_RELATED[D$TIPUSLESIO %in% c("Luxacio", "Fractura", "Esguinc", "Contusio", "Ferida", "Hemorragia", "Neumotorax","Caiguda","Muscular")] <- "Trauma"
D$TRAUMA_RELATED[!(D$TIPUSLESIO %in% c("Luxacio", "Fractura", "Esguinc", "Contusio", "Ferida", "Hemorragia", "Neumotorax","Caiguda","Muscular"))] <- "Others"

D$ACT_PERILLOSA = factor(D$ACT_PERILLOSA)
D$ACT_EQUIPAMENT_ADEQUAT = factor(D$ACT_EQUIPAMENT_ADEQUAT)
D$ACT_MOTIUS_JUSTIFICATS = factor(D$ACT_MOTIUS_JUSTIFICATS)

D$ACT_DAT_INICI <- as.POSIXct(D$ACT_DAT_INICI)
D$ACT_DAT_FI <- as.POSIXct(D$ACT_DAT_FI)

D$Duration = as.numeric(D$ACT_DAT_FI -D$ACT_DAT_INICI)
D$Duration[D$Duration > 2880] <- NA # Duration - more than 48h (2880 mins) considered missing data
D$Year = as.numeric(format(D$ACT_DAT_INICI, "%Y"))
D$Month = factor(format(D$ACT_DAT_INICI, "%m"))
D$Summer <- factor(ifelse(D$Month == "07" | D$Month == "07", 1, 0))
D$Day <- weekdays(D$ACT_DAT_INICI)
D$Day <- factor(D$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
D$Weekend <- factor(ifelse(D$Day %in% c("Saturday", "Sunday"), 1, 0))

D$Hour = factor(format(D$ACT_DAT_INICI, "%H"))

D <- D[!(D$Year == 2022 | D$Year == 2010), ]

##################
#  STUDY OF NAs  #
##################

row_na_count <- rowSums(is.na(D))

table(row_na_count)

colSums(is.na(D))[which(colSums(is.na(D)) > 0)]

table(is.na(D$ACT_X_UTM),D$Year)


#################################################################################################
#      Deleting points out of Catalonia and avoiding duplicates and creating point pattern      #
#################################################################################################

D_spatial = D[!is.na(D$ACT_X_UTM),]

D_spatial= D[!is.na(D$ACT_X_UTM),]

D_spatial= D_spatial[!is.na(D_spatial$ACT_X_UTM),]

window_cat <- as.owin(Cat_shape)
a<-inside.owin(D_spatial$ACT_X_UTM,D_spatial$ACT_Y_UTM, window_cat)

for (i in 1: nrow(D_spatial)) {
  if (a[i] == FALSE  ) { D_spatial$ACT_X_UTM[i] = NA; D_spatial$ACT_Y_UTM[i] = NA } 
}

D_spatial = D_spatial[!is.na(D_spatial$ACT_X_UTM),]

# Avoiding duplicated points
for(i in 1:nrow(D_spatial)){
  for(j in 1:nrow(D_spatial)){
    if(i<j){
      if(D_spatial$ACT_X_UTM[i]==D_spatial$ACT_X_UTM[j]) {D_spatial$ACT_X_UTM[j]=D_spatial$ACT_X_UTM[i]+runif(1,-1,1)}
      if(D_spatial$ACT_Y_UTM[i]==D_spatial$ACT_Y_UTM[j]) {D_spatial$ACT_Y_UTM[j]=D_spatial$ACT_Y_UTM[i]+runif(1,-1,1)}
    }
  }
}


wind_cat<-rescale(window_cat,1000)
D_spatial$ACT_X_UTM<-D_spatial$ACT_X_UTM/1000
D_spatial$ACT_Y_UTM<-D_spatial$ACT_Y_UTM/1000

#Create point pattern
pp_resc<-ppp(D_spatial$ACT_X_UTM,D_spatial$ACT_Y_UTM,window=wind_cat)
#plot(pp_resc)

#########################
#      DESCRIPTIVES     #
#########################

#descrTable(D)

#SEVERITY

table(D$LESIO)
table(D$LESIO,D$Year)
par(mar = c(5, 7, 4, 7))
plot(D$LESIO~D$Year,xlab="Year",ylab="Severity")


# Plot Severity by Type of Injury/Illness
df <- as.data.frame.matrix(CrossTable(D$TYPE_INJURY, D$LESIO, missing.include = FALSE)$t)
names(df)=levels(D$LESIO)
row.names(df)=levels(D$TYPE_INJURY)

df$TYPE_INJURY <- rownames(df)
severity_levels <- c("Unharmed", "Injured/Ill", "Life Risk", "Death")
df_long <- gather(df, LESIO, frequency, -TYPE_INJURY)
df_long$LESIO <- factor(df_long$LESIO, levels = severity_levels)
custom_colors <- c("green", "yellow", "orange", "red")

death <- df_long[df_long$LESIO == "Death", ]$frequency
total <- aggregate(frequency ~ TYPE_INJURY, data = df_long, sum)$frequency
prop_death <- death / total

y_position <- max(df_long$frequency) / 2  

ggplot(df_long, aes(x = TYPE_INJURY, y = frequency, fill = LESIO)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Injury by Severity",
       x = "Injury Type",
       y = "Frequency",
       fill = "Severity") +
  geom_text(data = subset(df_long, LESIO == "Death"),
            aes(label = paste0(round(prop_death * 100, 2), "%")), 
            color = "black",
            vjust = 0.3, hjust = 0, angle = 75, y = y_position) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle =-45 , hjust = 0)) + 
  scale_fill_manual(values = custom_colors)


# Correlation
names<-c("Severity","Activity","Dangerous","Equipment","Group","Gender","Altitude","Duration","Year","Weekend","Summer")
cor=as.matrix(cbind(D$LESIO, D$ACT_NIV2_COD, D$ACT_PERILLOSA, D$ACT_EQUIPAMENT_ADEQUAT, D$GRUP, D$SEX, D$Altitude, D$Duration, D$Year, D$Weekend, D$Summer))
colnames(cor)= names
correlation_table<-round(rcorr(cor,type = c("spearman"))$r,2)
 round(rcorr(cor,type = c("spearman"))$P,4)

 write.table(correlation_table, file = "correlation_table.txt", sep = "\t", quote = FALSE)

##################################
#       compareGroups            #
##################################

res <- compareGroups(LESIO ~ ACT_NIV2_COD +
                       ACT_PERILLOSA +
                       ACT_EQUIPAMENT_ADEQUAT +
                       GRUP +
                       SEX +
                       Weekend +
                       Summer, data = D,method=3,simplify=TRUE)

 res2 <- compareGroups(LESIO ~ Altitude +
                        Duration , data = D,method=NA)
 
 
summary(res)
tab <- createTable(res,show.p.mul=TRUE,show.n = TRUE)
ta <- createTable(res2,show.n = TRUE,show.ci =FALSE)

# export2word(tab, file='table1.docx')

res1 <- compareGroups(LESIO ~ TYPE_INJURY, data = D,compute.prop = TRUE)
tab1 <- createTable(res1,show.p.mul=TRUE,show.n = TRUE)

#######################
#      MAP PLOTS      #
#######################

#Creating variable distance to the centroid of the closest  subregion with more than 100.000 people

crs <- st_crs("+init=epsg:25831")
D_sf <- st_as_sf(D_spatial, coords = c("ACT_X_UTM", "ACT_Y_UTM"), crs = crs)
longlats <- st_transform(D_sf, crs = "+proj=longlat +datum=WGS84")
D_sf <- st_transform(longlats, crs = st_crs(Cat))


mun <- st_transform(mun, crs = st_crs(Cat))
Nat <- st_transform(Nat, crs = st_crs(Cat))


p =ggplot(Cat) +
  geom_sf(aes(),
          color = "grey70"
  ) +
  labs(title = "Catalunya") 
  theme_bw()
  
p + geom_sf(data = D_sf, aes(color = LESIO, fill = LESIO)) +
  scale_color_brewer(name = "Severity", type = "div", palette = 4) +
  scale_fill_brewer(name = "Severity", type = "div", palette = 4)


# Map points within polygons natural protected places
inter <- st_intersects(Nat,D_sf)
Nat$count <- lengths(inter)

ggplot(Cat)+ 
  geom_sf(aes(),
          color = "grey70"
  ) +
  geom_sf(data =Nat, aes(fill = count)) +
  scale_fill_continuous(  
    low = "white",  
    high = "red",
    name = "Cases" 
  ) +
  labs(title = "")

# Injured by region

inter <- st_intersects(Cat,D_sf)
Cat$count <- lengths(inter)

ggplot(Cat) + 
  geom_sf(aes(fill = count)) +
  scale_fill_continuous(  
    low = "white",  
    high = "black",
    name = "Cases" 
  ) +
  labs(title = "")

p + geom_sf(data = D_sf[D_sf$LESIO == "Z6",],aes(color=LESIO,fill=LESIO))+
  scale_color_brewer(type = "div", palette = 4) + 
  scale_fill_brewer(type = "div", palette = 4)

# Death by region
interZ6 <- st_intersects(Cat,D_sf[D_sf$LESIO == "Z6",])
Cat$countZ6 <- lengths(interZ6)

ggplot(Cat) + 
  geom_sf(aes(fill = countZ6)) +
  scale_fill_continuous(  
    low = "white",  
    high = "black",
    name = "Deaths",
    labels = scales::number_format(accuracy = 1)  # Ajusta el formato de los números en la leyenda
  ) +
  labs(title = "")

#Death Rate by region

Cat$DeathRate = Cat$countZ6/Cat$count

ggplot(Cat) + 
  geom_sf(aes(fill = DeathRate)) +
  scale_fill_continuous(  
    low = "white",  
    high = "black",
    name = "Death Rate",
    labels = scales::number_format(accuracy = .05)  # Ajusta el formato de los números en la leyenda
  ) +
  labs(title = "")


#############################
#      SPATIAL ANALYSIS     #
#############################


#####Gaussian Kernel approach########
h<-10.0  
dimx<-500
dimy<-500

# Dots density /km2
int_pp<-density.ppp(pp_resc,sigma=h,dimyx=c(dimx, dimy))

#Dot intensity by dot
int_pp_loc<-density.ppp(pp_resc,sigma=h,at="points")

plot(int_pp) 
#points(D_spatial$ACT_X_UTM,D_spatial$ACT_Y_UTM,pch=19)


#### K de Ripley and  Pair correlation functions analyzing the inhomogenous point patterns

out_Kin<-envelope(pp_resc, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp)))
plot(out_Kin$r, out_Kin$obs,type="l",axes=TRUE,xlim=c(0,15),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin$r,out_Kin$lo, col="red")
lines(out_Kin$r,out_Kin$hi, col="red")

# alpha = 0.05 

out_pcfin<-envelope(pp_resc, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=0.5, simulate=expression(rpoispp(int_pp)))
plot(out_pcfin$r, out_pcfin$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin$r,out_pcfin$lo, col="red")
lines(out_pcfin$r,out_pcfin$hi, col="red")

####################Bivariant patterns##########

#### TRAUMA ####

Lesio_marks<-factor(D_spatial$TRAUMA_RELATED)

## Creating the bivariant marked pattern
pp_resc_lesio <- ppp(D_spatial$ACT_X_UTM,D_spatial$ACT_Y_UTM,window=wind_cat,marks=Lesio_marks)
plot(pp_resc_lesio,axes="True")

##testing if its puntual 
is.multitype(pp_resc_lesio)

###Univariant analysis by mark###

##obtenier el patró puntual dels Trauma (només del Trauma) i el patró puntual dels Others

t1<-0; t2<-0
x1<-c(); y1<-c(); x2<-c(); y2<-c()
for(i in 1:length(D_spatial$ACT_X_UTM)){
  if(pp_resc_lesio$marks[i]=="Trauma"){
    t1=t1+1
    x1[t1]=D_spatial$ACT_X_UTM[i]
    y1[t1]=D_spatial$ACT_Y_UTM[i]
  }
  if(pp_resc_lesio$marks[i]=="Others"){
    t2=t2+1
    x2[t2]=D_spatial$ACT_X_UTM[i]
    y2[t2]=D_spatial$ACT_Y_UTM[i]
  }
}

##intensity function by mark
pp_trauma<-ppp(x1,y1,window=wind_cat)
pp_others<-ppp(x2,y2,window=wind_cat)

h<-10.0  #sigma<-bw.diggle 
dimx<-500
dimy<-500
int_pp_trauma_loc<-density.ppp(pp_trauma,sigma=h,at="points")
int_pp_trauma<-density.ppp(pp_trauma,sigma=h,dimyx=c(dimx, dimy))

int_pp_others_loc<-density.ppp(pp_others,sigma=h,at="points")
int_pp_others<-density.ppp(pp_others,sigma=h,dimyx=c(dimx, dimy))


## Trauma
out_Kin_trauma<-envelope(pp_trauma, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_trauma)))
plot(out_Kin_trauma$r, out_Kin_trauma$obs,type="l",axes=TRUE,xlim=c(0,25),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_trauma$r,out_Kin_trauma$lo, col="red")
lines(out_Kin_trauma$r,out_Kin_trauma$hi, col="red")

out_pcfin_trauma<-envelope(pp_trauma, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_trauma)))
plot(out_pcfin_trauma$r, out_pcfin_trauma$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_trauma$r,out_pcfin_trauma$lo, col="red")
lines(out_pcfin_trauma$r,out_pcfin_trauma$hi, col="red")

## Others

out_Kin_others<-envelope(pp_others, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_others)))
plot(out_Kin_others$r, out_Kin_others$obs,type="l",axes=TRUE,xlim=c(0,25),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_others$r,out_Kin_others$lo, col="red")
lines(out_Kin_others$r,out_Kin_others$hi, col="red")

out_pcfin_others<-envelope(pp_others, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_others)))
plot(out_pcfin_others$r, out_pcfin_others$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_others$r,out_pcfin_others$lo, col="red")
lines(out_pcfin_others$r,out_pcfin_others$hi, col="red")

##Bivariant analysis


###Random labelling

## H0: The process generating marks for Trauma is the same than for Others 

#Simulations
out<-pcfcross.inhom(pp_resc_lesio, i = "Trauma",  j = "Others", kernel="epanechnikov", correction="Ripley", bw=1.0, lambdaI=int_pp_trauma_loc, lambdaJ=int_pp_others_loc)
n<-length(D_spatial$ACT_X_UTM)
nsim<-199
nrank<-5

Envel1<-array(0,c(length(out$r),nsim))
dim(Envel1)<-c(length(out$r),nsim)

for(i1 in 1:nsim){
  print(i1)
  pp_resc_lesio_new<-pp_resc_lesio
  
  ##random permutation of marks over fixed points
  
  res<-sample(seq(1,n,1),n) 
  marks<-c()
  
  ##Make sure marks[i]=2 is Trauma
  for(i in 1:n) {
    marks[i]<-pp_resc_lesio_new$marks[res[i]]
    if(marks[i]==2) marks[i]="Trauma"
    if(marks[i]==1) marks[i]="Others"
  }
  
  pp_resc_lesio_new$marks<-factor(marks)
  
  t1<-0; t2<-0
  x1<-c(); y1<-c(); x2<-c(); y2<-c()
  for(i in 1:length(D_spatial$ACT_X_UTM)){
    if(pp_resc_lesio_new$marks[i]=="Trauma"){
      t1=t1+1
      x1[t1]= pp_resc_lesio_new$x[i]
      y1[t1]=pp_resc_lesio_new$y[i]
    }
    if(pp_resc_lesio_new$marks[i]=="Others"){
      t2=t2+1
      x2[t2]=pp_resc_lesio_new$x[i]
      y2[t2]=pp_resc_lesio_new$y[i]
    }
  }
  
  pp_trauma_sht<-ppp(x1,y1,window=wind_cat)
  pp_others_sht<-ppp(x2,y2,window=wind_cat)
  
  int_pp_trauma_loc_sht<-density.ppp(pp_trauma_sht,sigma=h,at="points")
  int_pp_others_loc_sht<-density.ppp(pp_others_sht,sigma=h,at="points")
  
  Env1<-pcfcross.inhom(pp_resc_lesio_new, i = "Trauma",  j = "Others", kernel="epanechnikov", correction="Ripley", bw=1.0, lambdaI=int_pp_trauma_loc_sht, lambdaJ=int_pp_others_loc_sht)
  Envel1[,i1]=Env1$iso
}

MaxEnv1<-c(length(out$r))
MinEnv1<-c(length(out$r))


alpha<-2*nrank/(nsim+1)
paste("alpha value:",paste(alpha,collapse=","))


for(i in 1:length(out$r))   MaxEnv1[i]<-sort(Envel1[i,])[nsim-nrank+1]
for(i in 1:length(out$r))   MinEnv1[i]<-sort(Envel1[i,])[nrank]


plot(out$r, out$iso,type="l",axes=TRUE, xlim=c(0.0,10), ylim=c(0,10),xlab="r - Distance (km)",ylab="Observed k(r)") #,ylim=c(0,80), xlim=c(0.0,2))
lines(out$r,MaxEnv1, col="red")
lines(out$r,MinEnv1, col="red")


### Random shift

#H0: spatial independence of the marks

#Simulations
sht<-0.05*(int_pp$xrange[2]-int_pp$xrange[1])
nsim<-199
nrank<-5

Envel<-array(0,c(length(out$r),nsim))
dim(Envel)<-c(length(out$r),nsim)

for(i1 in 1:nsim){
  print(i1)
  pp_resc_lesio_new<-pp_resc_lesio
  xnew<-c(); ynew<-c()
  for(j in 1:length(D_spatial$ACT_X_UTM)){
    for(j1 in 1:100000000){  
      xnew[j]<-pp_resc_lesio$x[j]+runif(1,-sht,sht)
      ynew[j]<-pp_resc_lesio$y[j]+runif(1,-sht,sht)
      if(inside.owin(xnew[j], ynew[j], wind_cat)==TRUE) break
    }
  }
  
  pp_resc_lesio_new$x<-xnew
  pp_resc_lesio_new$y<-ynew
  
  t1<-0; t2<-0
  x1<-c(); y1<-c(); x2<-c(); y2<-c()
  for(i in 1:length(D_spatial$ACT_X_UTM)){
    if(pp_resc_lesio_new$marks[i]=="Trauma"){
      t1=t1+1
      x1[t1]= pp_resc_lesio_new$x[i]
      y1[t1]=pp_resc_lesio_new$y[i]
    }
    if(pp_resc_lesio_new$marks[i]=="Others"){
      t2=t2+1
      x2[t2]=pp_resc_lesio_new$x[i]
      y2[t2]=pp_resc_lesio_new$y[i]
    }
  }
  
  pp_trauma_sht<-ppp(x1,y1,window=wind_cat)
  pp_others_sht<-ppp(x2,y2,window=wind_cat)
  
  int_pp_trauma_loc_sht<-density.ppp(pp_trauma_sht,sigma=h,at="points")
  int_pp_others_loc_sht<-density.ppp(pp_others_sht,sigma=h,at="points")
  
  Env<-pcfcross.inhom(pp_resc_lesio_new, i = "Trauma",  j = "Others", kernel="epanechnikov", correction="Ripley", bw=1.0, lambdaI=int_pp_trauma_loc_sht, lambdaJ=int_pp_others_loc_sht)
  Envel[,i1]=Env$iso
}

MaxEnv<-c(length(out$r))
MinEnv<-c(length(out$r))


alpha<-2*nrank/(nsim+1)
paste("alpha value:",paste(alpha,collapse=","))


for(i in 1:length(out$r))   MaxEnv[i]<-sort(Envel[i,])[nsim-nrank+1]
for(i in 1:length(out$r))   MinEnv[i]<-sort(Envel[i,])[nrank]


plot(out$r, out$iso,type="l",axes=TRUE, xlim=c(0.0,20),xlab="r - Distance (km)",ylab="Observed k(r)") #,ylim=c(0,80), xlim=c(0.0,2))
lines(out$r,MaxEnv, col="red")
lines(out$r,MinEnv, col="red")

#Ploting all simulations

plot(out$r, Envel[,1],type="l",axes=TRUE, xlim=c(0.0,5),ylim=c(0,5))
for(i in 2:199){
  lines(out$r,Envel[,i], col="red")
}
lines(out$r,MaxEnv, col="blue",lwd=2)
lines(out$r,MinEnv, col="blue",lwd=2)


#############################
#        Severity           #
#############################

##Creating the multivariate puntual pattern
pp_sev_lesio <- ppp(D_spatial$ACT_X_UTM,D_spatial$ACT_Y_UTM,window=wind_cat,marks=D_spatial$LESIO)
plot(pp_sev_lesio,axes="True")

x1 = D_spatial$ACT_X_UTM[which(D_spatial$LESIO == "Unharmed")]
y1 = D_spatial$ACT_Y_UTM[which(D_spatial$LESIO == "Unharmed")]

x2 = D_spatial$ACT_X_UTM[which(D_spatial$LESIO == "Injured/Ill")]
y2 = D_spatial$ACT_Y_UTM[which(D_spatial$LESIO == "Injured/Ill")]

x3 = D_spatial$ACT_X_UTM[which(D_spatial$LESIO == "Life Risk")]
y3 = D_spatial$ACT_Y_UTM[which(D_spatial$LESIO == "Life Risk")]

x4 = D_spatial$ACT_X_UTM[which(D_spatial$LESIO == "Death")]
y4 = D_spatial$ACT_Y_UTM[which(D_spatial$LESIO == "Death")]


###Intensity function by mark
pp_uninjured<-ppp(x1,y1,window=wind_cat)
pp_injured<-ppp(x2,y2,window=wind_cat)
pp_vitalrisk<-ppp(x3,y3,window=wind_cat)
pp_death<-ppp(x4,y4,window=wind_cat)

h<-10.0 
dimx<-500
dimy<-500

int_pp_uninjured_loc<-density.ppp(pp_uninjured,sigma=h,at="points")
int_pp_uninjured<-density.ppp(pp_uninjured,sigma=h,dimyx=c(dimx, dimy))

int_pp_injured_loc<-density.ppp(pp_injured,sigma=h,at="points")
int_pp_injured<-density.ppp(pp_injured,sigma=h,dimyx=c(dimx, dimy))

int_pp_vitalrisk_loc<-density.ppp(pp_vitalrisk,sigma=h,at="points")
int_pp_vitalrisk<-density.ppp(pp_vitalrisk,sigma=h,dimyx=c(dimx, dimy))

int_pp_death_loc<-density.ppp(pp_death,sigma=h,at="points")
int_pp_death<-density.ppp(pp_death,sigma=h,dimyx=c(dimx, dimy))



##Uninjured
out_Kin_uninj<-envelope(pp_uninjured, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_uninjured)))
plot(out_Kin_uninj$r, out_Kin_uninj$obs,type="l",axes=TRUE,xlim=c(0,15),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_uninj$r,out_Kin_uninj$lo, col="red")
lines(out_Kin_uninj$r,out_Kin_uninj$hi, col="red")

out_pcfin_uninj<-envelope(pp_uninjured, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_uninjured)))
plot(out_pcfin_uninj$r, out_pcfin_uninj$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_uninj$r,out_pcfin_uninj$lo, col="red")
lines(out_pcfin_uninj$r,out_pcfin_uninj$hi, col="red")


##Injured
out_Kin_inj<-envelope(pp_injured, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_injured)))
plot(out_Kin_inj$r, out_Kin_inj$obs,type="l",axes=TRUE,xlim=c(0,15),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_inj$r,out_Kin_inj$lo, col="red")
lines(out_Kin_inj$r,out_Kin_inj$hi, col="red")

out_pcfin_inj<-envelope(pp_injured, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_injured)))
plot(out_pcfin_inj$r, out_pcfin_inj$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_inj$r,out_pcfin_inj$lo, col="red")
lines(out_pcfin_inj$r,out_pcfin_inj$hi, col="red")


##Vital Risk
out_Kin_vrisk<-envelope(pp_vitalrisk, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_vitalrisk)))
plot(out_Kin_vrisk$r, out_Kin_vrisk$obs,type="l",axes=TRUE,xlim=c(0,70),ylim=c(0,50),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_vrisk$r,out_Kin_inj$lo, col="red")
lines(out_Kin_vrisk$r,out_Kin_inj$hi, col="red")

out_pcfin_vrisk<-envelope(pp_vitalrisk, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_vitalrisk)))
plot(out_pcfin_vrisk$r, out_pcfin_vrisk$obs,type="l",axes=TRUE,xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_vrisk$r,out_pcfin_vrisk$lo, col="red")
lines(out_pcfin_vrisk$r,out_pcfin_vrisk$hi, col="red")


##Death
out_Kin_death<-envelope(pp_death, Kinhom, nsim = 199, nrank = 5, sigma=10, simulate=expression(rpoispp(int_pp_death)))
plot(out_Kin_death$r, out_Kin_death$obs,type="l",axes=TRUE,xlim=c(0,15),ylim=c(0,1000),xlab="r - Distance (km)",ylab="Observed K(r)")
lines(out_Kin_death$r,out_Kin_death$lo, col="red")
lines(out_Kin_death$r,out_Kin_death$hi, col="red")

out_pcfin_death<-envelope(pp_death, pcfinhom, nsim = 199, nrank = 5, sigma=10, bw=1.0, simulate=expression(rpoispp(int_pp_death)))
plot(out_pcfin_death$r, out_pcfin_death$obs,type="l",axes=TRUE,xlim=c(0,50),ylim=c(-0.1,5),xlab="r - Distance (km)",ylab="Observed g(r)")
lines(out_pcfin_death$r,out_pcfin_death$lo, col="red")
lines(out_pcfin_death$r,out_pcfin_death$hi, col="red")


#############################
#  LOGISTIC ORDINAL REG     #
#############################

D$LESIO = ordered(D$LESIO)
D$GENDER <- droplevels(D$SEX, exclude = "")
D$GENDER <- as.character(D$GENDER)
D$GENDER[D$GENDER == "Mixed"] <- NA
D$GENDER <- factor(D$GENDER)

D$GROUP = D$GRUP
D$GENDER <- as.character(D$GENDER)
D$GROUP[D$GROUP == "Competition"] <- "Group"
D$GROUP = factor(D$GROUP)

Dr=na.omit(D[,c("Year","Weekend","LESIO","Summer", "GENDER", "GROUP", "ACT_PERILLOSA", "ACT_NIV2_COD", "Duration", "ACT_EQUIPAMENT_ADEQUAT","LESIO","Altitude")]) #2161

#2285

Dr$Year = factor(Dr$Year)

#Time
mt <- polr(LESIO ~  Year + Weekend + Summer + Duration , data = Dr, Hess=TRUE,na.action = na.exclude)
brant(mt)
vif(mt)

# Other variables
mo <- polr(LESIO ~  Altitude + GENDER  + ACT_NIV2_COD + GROUP + ACT_PERILLOSA + ACT_EQUIPAMENT_ADEQUAT
, data = Dr, Hess=TRUE,na.action = na.exclude)
brant(mo)
round(vif(mo),4)

#########
#TIME

MT<- vglm(LESIO ~  Year + Weekend + Summer + Duration  , family=cumulative(parallel=TRUE),data=Dr,na.action=na.omit,model=TRUE)

MTNP<- vglm(LESIO ~ Year + Weekend + Summer + Duration, family=cumulative(parallel=FALSE~Summer + Weekend),data=Dr,na.action=na.omit,model=TRUE)

#Interactions

MTI<- vglm(LESIO ~  Year + Weekend + Summer + Duration + 
                    Year:Weekend + Year:Summer + Year:Duration + 
                    Weekend:Summer + Weekend:Duration +
                    Summer:Duration 
             , family=cumulative(parallel=TRUE),data=Dr,na.action=na.omit,model=TRUE)

MTNPI<- vglm(LESIO ~ Year + Weekend + Summer + Duration + 
               Year:Weekend + Year:Summer + Year:Duration + 
               Weekend:Summer + Weekend:Duration +
               Summer:Duration 
             , family=cumulative(parallel=FALSE~Summer + Weekend),data=Dr,na.action=na.omit,model=TRUE)

#NONE OF INTERACTIONS SIGNIFICANT

#Deleted Summer and Duration

MTF<- vglm(LESIO ~  Year + Weekend , family=cumulative(parallel=TRUE),data=Dr,na.action=na.omit,model=TRUE)

#Deleted Summer and Duration

MTNPF<- vglm(LESIO ~ Year + Weekend , family=cumulative(parallel=FALSE~ Weekend),data=Dr,na.action=na.omit,model=TRUE)

###################
# ACCIDENT RELATED


#DELETING EQUIPMENT ADEQUACY and DANGEROUS multiconlinearity

MO <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP , family=cumulative(parallel=TRUE), data=Dr,na.action=na.omit,model=TRUE)

MONP <- vglm(LESIO ~  Altitude + GENDER  + ACT_NIV2_COD + GROUP , family=cumulative(parallel=FALSE~Altitude), data=Dr,na.action=na.omit,model=TRUE)

#Interactions

MOI <- vglm(LESIO ~  Altitude + GENDER  + ACT_NIV2_COD + GROUP +
              Altitude:ACT_NIV2_COD + Altitude:GROUP 
           , family=cumulative(parallel=TRUE), data=Dr,na.action=na.omit,model=TRUE)

#Altitude:ACT_NIV2_COD significant

MONPI <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP +
                Altitude:ACT_NIV2_COD + Altitude:GROUP 
             , family=cumulative(parallel=FALSE~Altitude), data=Dr,na.action=na.omit,model=TRUE)

#Final

####

FINAL <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP +
                Altitude:ACT_NIV2_COD + Altitude:GROUP+ 
                Year + Weekend  , family=cumulative(parallel=TRUE), data=Dr,na.action=na.omit,model=TRUE)

FINALNP <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP +
                Altitude:ACT_NIV2_COD + Altitude:GROUP+ 
                Year + Weekend  , family=cumulative(parallel=FALSE~Altitude + Weekend), data=Dr,na.action=na.omit,model=TRUE)

FINALI <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP + Year + Weekend +
                  Altitude:ACT_NIV2_COD + Altitude:GROUP + 
                   ACT_NIV2_COD:Weekend 
                   , family=cumulative(parallel=TRUE), data=Dr,na.action=na.omit,model=TRUE)

FINALNPI <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP + Year + Weekend +
                 Altitude:ACT_NIV2_COD + Altitude:GROUP + 
                 ACT_NIV2_COD:Weekend 
               , family=cumulative(parallel=FALSE~Altitude + Weekend), data=Dr,na.action=na.omit,model=TRUE)


#Table AIC PseudoR2 Deviance LogLik

names <- c("MT","MTNP","MTF","MTNPF","MO","MONP","MOI","MONPI","FINAL","FINALNP","FINALI","FINALNPI")

AIC_values <- list()
Nagelkerke_values <- list()
Deviance_values <- list()
LogLik_values <- list()


for (name in names) {
  fit <- get(name)  
 
  AIC_values[[name]] <- AIC(fit)
  Nagelkerke_values[[name]] <- PseudoR2(fit, which = "Nagelkerke")
  
  deviance_value <- fit@criterion$deviance
  loglik_value <- fit@criterion$loglikelihood
  
  Deviance_values[[name]] <- deviance_value
  LogLik_values[[name]] <- loglik_value
}

results <- data.frame(
  AIC = unlist(AIC_values),
  NagelkerkeR2 = unlist(Nagelkerke_values),
  Deviance = unlist(Deviance_values),
  LogLik = unlist(LogLik_values)
)
results

#### CONTRAST

#lrtest(MT,MTF)
#lrtest(MT,FINAL)
MT.FINAL=c(lrtest(MT,FINAL)@Body$Chisq[2],round(lrtest(MT,FINAL)@Body$`Pr(>Chisq)`[2],4))
#lrtest(MO,MOI)
#lrtest(MOI,FINAL)
MOI.FINAL=c(lrtest(MOI,FINAL)@Body$Chisq[2],round(lrtest(MOI,FINAL)@Body$`Pr(>Chisq)`[2],4))

#lrtest(FINAL,FINALI)
FINAL.FINALI=c(lrtest(FINAL,FINALI)@Body$Chisq[2],round(lrtest(FINAL,FINALI)@Body$`Pr(>Chisq)`[2],4))

#lrtest(MTNPF,MTNP)
#lrtest(MTNPF,FINALNP)
MTNPF.FINALNP=c(lrtest(MTNPF,FINALNP)@Body$Chisq[2],round(lrtest(MTNPF,FINALNP)@Body$`Pr(>Chisq)`[2],4))
#lrtest(MONP,MONPI)
#lrtest(MONPI,FINALNP)
MONPI.FINALNP=c(lrtest(MONPI,FINALNP)@Body$Chisq[2],round(lrtest(MONPI,FINALNP)@Body$`Pr(>Chisq)`[2],4))

#lrtest(FINALNP,FINALNPI)
FINALNP.FINALNPI=c(lrtest(FINALNP,FINALNPI)@Body$Chisq[2],round(lrtest(FINALNP,FINALNPI)@Body$`Pr(>Chisq)`[2],4))

#lrtest(FINAL,FINALNP)
FINAL.FINALNP=c(lrtest(FINAL,FINALNP)@Body$Chisq[2],round(lrtest(FINAL,FINALNP)@Body$`Pr(>Chisq)`[2],4))

summary.LR =rbind(MT.FINAL,MOI.FINAL,FINAL.FINALI,MTNPF.FINALNP,MONPI.FINALNP,FINALNP.FINALNPI,FINAL.FINALNP)
colnames(summary.LR) = c("Chisq","p")
summary.LR

summary(FINALNP)


# Residuals analysis

par(mfrow=c(2,3))
plotvglm(FINALNP, which = "(All)")

par(mfrow=c(1,3))
hatplot(FINALNP,cex=.2,col="black")


####
#Hat
cutoff<-3*(dim(hatvalues(FINALNP))[2]+1)/dim(hatvalues(FINALNP))[1]

table(hatvalues(FINALNP)[,1]>= cutoff)
table(hatvalues(FINALNP)[,2]>= cutoff)
table(hatvalues(FINALNP)[,3]>= cutoff)

resid=residuals(FINALNP,type="pearson")
# quantile(resid, 0.95)

table(residuals(FINALNP,type="pearson")[,1]> quantile(resid, 0.95) & hatvalues(FINALNP)[,1]>= cutoff) 
table(residuals(FINALNP,type="pearson")[,2]> quantile(resid, 0.95) & hatvalues(FINALNP)[,2]>= cutoff) 
table(residuals(FINALNP,type="pearson")[,3]> quantile(resid, 0.95) & hatvalues(FINALNP)[,3]>= cutoff)

r=which((residuals(FINALNP,type="pearson")[,1]> quantile(resid, 0.95) & hatvalues(FINALNP)[,1]>= cutoff) |
        (residuals(FINALNP,type="pearson")[,2]> quantile(resid, 0.95) & hatvalues(FINALNP)[,2]>= cutoff) |
        (residuals(FINALNP,type="pearson")[,3]> quantile(resid, 0.95) & hatvalues(FINALNP)[,3]>= cutoff))


Drf=Dr[-r,]
Drnf=Dr[r,]

#Checked model

FINALNP_Check <- vglm(LESIO ~ Altitude + GENDER  + ACT_NIV2_COD + GROUP +
                  Altitude:ACT_NIV2_COD + Altitude:GROUP+ 
                  Year + Weekend  , family=cumulative(parallel=FALSE~Altitude + Weekend), data=Drf,na.action=na.omit,model=TRUE)
summary(FINALNP_Check)

- AIC(FINALNP_Check) + AIC(FINALNP)
- PseudoR2(FINALNP_Check, which = "Nagelkerke") + PseudoR2(FINALNP, which = "Nagelkerke")
-FINALNP_Check@criterion$deviance + FINALNP@criterion$deviance
-FINALNP_Check@criterion$loglikelihood + FINALNP@criterion$loglikelihood

#Describing exlcuded observations
descrTable(Drnf)

round(cbind(b@coef3,confint(FINALNP_Check),exp(b@coefficients)),4)

######################################################################
######################################################################
##                                                                  ##
##                        SUPPLEMENTAL                              ##
##                                                                  ##
######################################################################
######################################################################

##################################
#   DESCRIPTIVE GRAPHS by SEX    #
##################################

D_clean <- D
D_clean <- D_clean[D_clean$SEX %in% c("Female", "Male"), ]

# Activity by sex

Activity<-data.frame(cbind(Sex=c(rep("Female",length(names(table(D_clean$ACT_NIV2_COD)))),rep("Male",length(names(table(D_clean$ACT_NIV2_COD))))),
                           Prop=c(prop.table(table(D_clean$ACT_NIV2_COD[which(D_clean$SEX=="Female")],D_clean$SEX[which(D_clean$SEX=="Female")]))[,1]*100,
                                  prop.table(table(D_clean$ACT_NIV2_COD[which(D_clean$SEX=="Male")],D_clean$SEX[which(D_clean$SEX=="Male")]))[,2]*100),
                           Act = c(names(table(D_clean$ACT_NIV2_COD)),names(table(D_clean$ACT_NIV2_COD))
                           )))

Activity$Prop <- as.numeric(as.character(Activity$Prop))


ggplot(Activity, aes(x = Act, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Activities by Gender",
       y = "%",
       x = "Activity")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by sex

prop_female_GRUP <- prop.table(table(D_clean$GRUP[D_clean$SEX == "Female"])) * 100
prop_male_GRUP <- prop.table(table(D_clean$GRUP[D_clean$SEX == "Male"])) * 100

GRUP_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_GRUP)),
  Prop = c(prop_female_GRUP, prop_male_GRUP),
  GRUP = rep(names(prop_female_GRUP), 2)
)

GRUP_df$Prop <- as.numeric(as.character(GRUP_df$Prop))
GRUP_df <- GRUP_df[order(GRUP_df$Prop), ]

ggplot(GRUP_df, aes(x = GRUP, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Groups by Gender",
       y = "%",
       x = "Group")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Severity by Sex

prop_female_LESIO <- prop.table(table(D_clean$LESIO[D_clean$SEX == "Female"])) * 100
prop_male_LESIO <- prop.table(table(D_clean$LESIO[D_clean$SEX == "Male"])) * 100

LESIO_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_LESIO)),
  Prop = c(prop_female_LESIO, prop_male_LESIO),
  LESIO = rep(names(prop_female_LESIO), 2)
)

LESIO_df$Prop <- as.numeric(as.character(LESIO_df$Prop))

LESIO_df <- LESIO_df[order(LESIO_df$Prop), ]

ggplot(LESIO_df, aes(x = LESIO, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Severity by Gender",
       y = "%",
       x = "Severity")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Dangerous by sex

prop_female_perilous <- prop.table(table(D_clean$ACT_PERILLOSA[D_clean$SEX == "Female"])) * 100
prop_male_perilous <- prop.table(table(D_clean$ACT_PERILLOSA[D_clean$SEX == "Male"])) * 100

perilous_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_perilous)),
  Prop = c(prop_female_perilous, prop_male_perilous),
  Perilous = rep(names(prop_female_perilous), 2)
)

perilous_df$Prop <- as.numeric(as.character(perilous_df$Prop))

perilous_df <- perilous_df[order(perilous_df$Prop), ]

ggplot(perilous_df, aes(x = Perilous, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Perilous Activity by Gender",
       y = "%",
       x = "Dangerous Activity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Equipment adequacy


prop_female_adequate <- prop.table(table(D_clean$ACT_EQUIPAMENT_ADEQUAT[D_clean$SEX == "Female"])) * 100
prop_male_adequate <- prop.table(table(D_clean$ACT_EQUIPAMENT_ADEQUAT[D_clean$SEX == "Male"])) * 100

adequate_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_adequate)),
  Prop = c(prop_female_adequate, prop_male_adequate),
  Adequate = rep(names(prop_female_adequate), 2)
)

adequate_df$Prop <- as.numeric(as.character(adequate_df$Prop))

adequate_df <- adequate_df[order(adequate_df$Prop), ]

ggplot(adequate_df, aes(x = Adequate, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Equipment Adequacy by Gender",
       y = "%",
       x = "Equipment Adequacy")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Injury Type

prop_female_injury <- prop.table(table(D_clean$TYPE_INJURY[D_clean$SEX == "Female"])) * 100
prop_male_injury <- prop.table(table(D_clean$TYPE_INJURY[D_clean$SEX == "Male"])) * 100

injury_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_injury)),
  Prop = c(prop_female_injury, prop_male_injury),
  Injury_Type = rep(names(prop_female_injury), 2)
)

injury_df$Prop <- as.numeric(as.character(injury_df$Prop))

injury_df <- injury_df[order(injury_df$Injury_Type), ]

ggplot(injury_df, aes(x = Injury_Type, y = Prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Injury Type by Gender",
       y = "%",
       x = "Injury Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################
#    TIME/SEX    #
##################

# Hour

prop_female_hour <- prop.table(table(D_clean$Hour[D_clean$SEX == "Female"])) * 100
prop_male_hour <- prop.table(table(D_clean$Hour[D_clean$SEX == "Male"])) * 100

hour_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_hour)),
  Prop = c(prop_female_hour, prop_male_hour),
  Hour = rep(names(prop_female_hour), 2)
)

hour_df$Prop <- as.numeric(as.character(hour_df$Prop))

hour_df <- hour_df[order(hour_df$Prop), ]

ggplot(hour_df, aes(x = Hour, y = Prop, color = Sex, group = Sex)) +
  geom_line() +  
  labs(title = "Comparison of Hour by Gender",
       y = "%",
       x = "Hour") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Day

prop_female_day <- prop.table(table(D_clean$Day[D_clean$SEX == "Female"])) * 100
prop_male_day <- prop.table(table(D_clean$Day[D_clean$SEX == "Male"])) * 100

day_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_day)),
  Prop = c(prop_female_day, prop_male_day),
  Day = rep(names(prop_female_day), 2)
)

day_df$Prop <- as.numeric(as.character(day_df$Prop))

day_df$Day <- factor(day_df$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(day_df, aes(x = Day, y = Prop, color = Sex, group = Sex)) +
  geom_line() +  
  labs(title = "Comparison of Day by Gender",
       y = "%",
       x = "Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Month

prop_female_month <- prop.table(table(D_clean$Month[D_clean$SEX == "Female"])) * 100
prop_male_month <- prop.table(table(D_clean$Month[D_clean$SEX == "Male"])) * 100

month_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_month)),
  Prop = c(prop_female_month, prop_male_month),
  Month = rep(names(prop_female_month), 2)
)

month_df$Prop <- as.numeric(as.character(month_df$Prop))

month_df <- month_df[order(match(month_df$Month, month.name)), ]

ggplot(month_df, aes(x = Month, y = Prop, color = Sex, group = Sex)) +
  geom_line() +  
  labs(title = "Comparison of Month by Gender",
       y = "%",
       x = "Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Year

prop_female_year <- prop.table(table(D_clean$Year[D_clean$SEX == "Female"])) * 100
prop_male_year <- prop.table(table(D_clean$Year[D_clean$SEX == "Male"])) * 100

year_df <- data.frame(
  Sex = rep(c("Female", "Male"), each = length(prop_female_year)),
  Prop = c(prop_female_year, prop_male_year),
  Year = rep(names(prop_female_year), 2)
)

year_df$Prop <- as.numeric(as.character(year_df$Prop))
year_df <- year_df[order(year_df$Year), ]

ggplot(year_df, aes(x = Year, y = Prop, color = Sex, group = Sex)) +
  geom_line() +  
  labs(title = "Comparison of Year by Gender",
       y = "%",
       x = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


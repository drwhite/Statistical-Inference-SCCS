#--extract variables to be used from sccs, put in dataframe my_sccs--

 data(sccs)

 my_sccs<-data.frame(
 #--For dep_var, we sum variables measuring how much a society values children--
 #--can replace "sum" with "max"
 dep_var=apply(sccs[,c("v473","v474","v475","v476")],1,sum),
 socname=sccs$socname,
 socID=sccs$"sccs#",
 valchild=(sccs$v473+sccs$v474+sccs$v475+sccs$v476),
 cultints=sccs$v232,roots=(sccs$v233==5)*1,
 cereals=(sccs$v233==6)*1,
 gath=sccs$v203,
 hunt=sccs$v204,
 fish=sccs$v205,anim=sccs$v206,
 femsubs=sccs$v890,
 pigs=(sccs$v244==2)*1,
 milk=(sccs$v245>1)*1,
 plow=(sccs$v243>1)*1,
 bovines=(sccs$v244==7)*1,
 tree=(sccs$v233==4)*1,
 foodtrade=sccs$v819,
 foodscarc=sccs$v1685,
 ecorich=sccs$v857,
 popdens=sccs$v156,
 pathstress=sccs$v1260,
 CVrain=sccs$v1914/sccs$v1913,
 rain=sccs$v854,
 temp=sccs$v855,
 AP1=sccs$v921,
 AP2=sccs$v928,
 ndrymonth=sccs$v196,
 exogamy=sccs$v72,
 ncmallow=sccs$v227,
 famsize=sccs$v80,
 settype=sccs$v234,
 localjh=(sccs$v236-1),
 superjh=sccs$v237,
 moralgods=sccs$v238,
 fempower=sccs$v663,
 sexratio=1+(sccs$v1689>85)+(sccs$v1689>115),
 war=sccs$v1648,
 himilexp=(sccs$v899==1)*1,
 money=sccs$v155,
 wagelabor=sccs$v1732,
 migr=(sccs$v677==2)*1,
 brideprice=(sccs$v208==1)*1,
 nuclearfam=(sccs$v210<=3)*1,
 pctFemPolyg=sccs$v872)

 indep_vars<-c("AP1","AP2","CVrain","anim","bovines","brideprice","cereals","cultints","ecorich","exogamy","famsize","fempower","femsubs","fish","foodscarc","foodtrade","gath","himilexp","hunt","localjh","migr","milk","money","moralgods","ncmallow","ndrymonth","nuclearfam","pathstress","pctFemPolyg","pigs","plow","popdens","rain","roots","settype","sexratio","superjh","tree","wagelabor","war")

 restrict_vars=c("cultints","roots","fish","exogamy","settype","femsubs")

 library(foreign)
 #--Read in two weight matrices--
 Wll<-as.matrix(read.dta("./examples/data/langwm.dta")[,-1])
 Wdd<-as.matrix(read.dta("./examples/data/dist25wm.dta")[,c(-1,-2,-189)])

 load("./examples/data/vaux.Rdata",.GlobalEnv)
 my_aux = vaux
 row.names(my_aux)<-NULL
 #--remove the society name field--
 my_aux<-my_aux[,-28]

 name<-"how society values children"
 alias<-"children"

 model=list(name=name,
            alias=alias,
            data=my_sccs,
            aux_data=my_aux,
            prox_list=list(language=Wll,distance=Wdd),
            dep_var="dep_var",
            indep_vars=indep_vars,
            restrict_vars=restrict_vars)

 save(model,file=paste(alias,".Rdata",sep=""))

#Replication code for Education and Trust Blog Post
library(haven)
library(survey)

cb17<-read_dta("CB_2017_Regional_Only_Responses_22.02.18.dta")
geo17<-subset(cb17, cb17$COUNTRY==3)
designgeo17 <- svydesign(id=~PSU,weights=~INDWT, strat=~STRATUM, data=geo17)

##Cleaning up data
table(geo17$ETHNIC)
geo17$ETHNIC_r<-geo17$ETHNIC
geo17$ETHNIC_r[geo17$ETHNIC_r!=3]<-1
geo17$ETHNIC_r[geo17$ETHNIC_r==3]<-0
geo17$TRUEDUC_r<-geo17$TRUEDUC
geo17$TRUEDUC_r[geo17$TRUEDUC_r<=-1]<-NA
freq(geo17$EDUYRS)
geo17$EDUYRS_r<-geo17$EDUYRS
geo17$EDUYRS_r[geo17$EDUYRS_r<=-1]<-NA

geo17$OWNCOTV_r<-geo17$OWNCOTV
geo17$OWNCOTV_r[geo17$OWNCOTV_r<=-1]<-NA

geo17$OWNDIGC_r<-geo17$OWNDIGC
geo17$OWNDIGC_r[geo17$OWNDIGC_r<=-1]<-NA

geo17$OWNWASH_r<-geo17$OWNWASH
geo17$OWNWASH_r[geo17$OWNWASH<=-1]<-NA

geo17$OWNFRDG_r<-geo17$OWNFRDG
geo17$OWNFRDG_r[geo17$OWNFRDG<=-1]<-NA

geo17$OWNAIRC_r<-geo17$OWNAIRC
geo17$OWNAIRC_r[geo17$OWNAIRC_r<=-1]<-NA

geo17$OWNCARS_r<-geo17$OWNCARS
geo17$OWNCARS_r[geo17$OWNCARS_r<=-1]<-NA

geo17$OWNLNDP_r<-geo17$OWNLNDP
geo17$OWNLNDP_r[geo17$OWNLNDP_r<=-1]<-NA

geo17$OWNCELL_r<-geo17$OWNCELL
geo17$OWNCELL_r[geo17$OWNCELL_r<=-1]<-NA

geo17$OWNCOMP_r<-geo17$OWNCOMP
geo17$OWNCOMP_r[geo17$OWNCOMP_r<=-1]<-NA

geo17$assets<-(geo17$OWNCOTV_r+
                 geo17$OWNDIGC_r+
                 geo17$OWNWASH_r+
                 geo17$OWNFRDG_r+
                 geo17$OWNAIRC_r+
                 geo17$OWNCARS_r+
                 geo17$OWNLNDP_r+
                 geo17$OWNCELL_r+
                 geo17$OWNCOMP_r)

geo17$children<-(geo17$V4-geo17$V5)
table(geo17$children)
geo17$children_dummy<-geo17$children
geo17$children_dummy[geo17$children_dummy>=1]<-1

###model
designgeo17 <- svydesign(id=~PSU,weights=~INDWT, strat=~STRATUM, data=geo17)
edutrustmodel<-svyglm(TRUEDUC_r~EDUYRS_r+as.factor(STRATUM)+
                        children_dummy+
                        RESPSEX+
                        AGE+
                        ETHNIC_r+assets, design=designgeo17)
summary(edutrustmodel)
exp(edutrustmodel$coefficients)
##getting predicted probabilities

newdata1<-with(geo17, data.frame(EDUYRS_r=0:30,
                                 AGE=mean(AGE, na.rm=TRUE),
                                 RESPSEX=mean(RESPSEX, na.rm=TRUE),
                                 STRATUM=1,
                                 STRATUM=2,
                                 STRATUM=3,
                                 children_dummy=mean(children_dummy, na.rm=TRUE),
                                 ETHNIC_r=mean(ETHNIC_r, na.rm=TRUE),
                                 assets=mean(assets, na.rm=TRUE)))

preduiuct<-predict(edutrustmodel, newdata=newdata1, type = "resp", se.fit=TRUE)
plot(preduiuct)
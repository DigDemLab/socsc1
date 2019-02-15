##########################################
# Replication Data for Proksch, Lowe, Wäckerle, Soroka. (2018). Multilingual Sentiment Analysis: A New Approach to Measuring Conflict in Legislative Speeches. Legislative Studies Quarterly, Forthcoming.
##########################################

#Part 1: Multilingual Validation: SOTEU Debate in the EP
#This file produces the sentiment measures for the translated ropuus and dictionaries that can be merged with the handcoded data in 1.5.
# It also generates found words for selected speeches and positions of party leaders, both fo the appendix.

##########################
# Intro
library(tidyverse)
require(devtools)
#install_version("quanteda", version = "1.1.1", repos = "http://cran.us.r-project.org") #all analysis is run on quanteda version 1.1.1
library(quanteda)
library(irr)
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Load the protocols
load("1_soteu_2010_protocols.RData")

# Load the dictionaries
load("auto_dictionaries_lsd.RData")
load("lsde_frenche_germane.RData")
load("auto_dictionaries_sentiws.RData")
frnch.lsd=dictionary(file = "frlsd.cat")
protocol.2010.bg
# Load the metadata for speeches
meta.2010.protocol=read.csv("1_metadata_2010.csv",sep=",",stringsAsFactors = F,encoding = "UTF-8")
names(meta.2010.protocol)[1]="Name"

############################################################
### 2010 Groups Lexicoder Dictionaries
############################################################
languages=c("bg","cs","da","de","el","en","es","et","fi","fr","hu","it","lt","lv","nl","pl","pt","ro","sk","sl","sv")

for(i in languages){
  cat("Calculating sentiment for:", i,"\n")
  if(i==languages[1]){
sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("extendeddict_",i))))
assign("groups_2010",cbind(sentisave,log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }else{
    sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                   remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("extendeddict_",i))))
    assign("groups_2010",cbind(get("groups_2010"),log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }
}
names(groups_2010)=c("Name","pos","neg",paste0("Sentiment_",languages))

############################################################
### 2010 Groups Lexicoder Dictionaries extended
############################################################
languages=c("de","fr")

for(i in languages){
  cat("Calculating sentiment for:", i,"\n")
  if(i==languages[1]){
    sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                   remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("extendeddict_",i,"_e"))))
    assign("groups_2010_e",cbind(sentisave,log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }else{
    sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                   remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("extendeddict_",i,"_e"))))
    assign("groups_2010_e",cbind(get("groups_2010_e"),log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }
}
names(groups_2010_e)=c("Name","pos","neg",paste0("Sentiment_",languages,"_e"))

############################################################
### 2010 Groups SentiWS Dictionaries
############################################################
languages=c("bg","cs","da","de","el","en","et","fi","fr","hu","it","lv","nl")

for(i in languages){
  cat("Calculating sentiment for:", i,"\n")
  if(i==languages[1]){
    sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                   remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("sentiws_",i))))
    assign("groups_2010_ws",cbind(sentisave,log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }else{
    sentisave=data.frame(dfm(corpus_subset(get(paste0("protocol.2010.",i)),Group!="President"),
                                                   remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = get(paste0("sentiws_",i))))
    assign("groups_2010_ws",cbind(get("groups_2010_ws"),log((sentisave$pos+0.5)/(sentisave$neg+0.5))))
  }
}
names(groups_2010_ws)=c("Name","pos","neg",paste0("Sentiment_",languages,"_ws"))

groups_2010=left_join(groups_2010,groups_2010_e%>%select(-c(pos,neg)))
groups_2010=left_join(groups_2010,groups_2010_ws%>%select(-c(pos,neg)))

############################################################
### 2010 French LSD 
############################################################

french.lsd.data=data.frame(dfm(corpus_subset(protocol.2010.fr,Group!="President"),
                          remove_punct = TRUE,remove_numbers=TRUE,groups="Group", dictionary = frnch.lsd))
french.lsd.data$Sentiment=log((french.lsd.data$POSITIVE+0.5)/(french.lsd.data$NEGATIVE+0.5))
names(french.lsd.data)=c("Name","pos","neg","Sentiment_frnch_lsd")
groups_2010=left_join(groups_2010,french.lsd.data%>%select(-c(pos,neg)))

################################################################
# Add in human coding results
################################################################

coder_a_2010=read.csv("1_coding_a_2010.csv",sep=",",stringsAsFactors = F)
names(coder_a_2010)[names(coder_a_2010)=="Coding"]="coding.a"
names(coder_a_2010)[1]="Number"

coder_b_2010=read.csv("1_coding_b_2010.csv",sep=",",stringsAsFactors = F)
names(coder_b_2010)[names(coder_b_2010)=="Coding"]="coding.b"
names(coder_b_2010)[1]="Number"

coder_c_2010=read.csv("1_coding_c_2010.csv",sep=",",stringsAsFactors = F)
names(coder_c_2010)[names(coder_c_2010)=="Coding"]="coding.c"
names(coder_c_2010)[1]="Number"

# Combine coders leaving empty rows where the coders weren't assigned to code (each coder was assigned two thirds of all paragraphs)
human_coding_2010=full_join(coder_a_2010,coder_b_2010,by="Number")
# Fill in the missing texts and information from when coder a wasn't assigned the text
human_coding_2010$Text.x[is.na(human_coding_2010$Text.x)]=human_coding_2010$Text.y[is.na(human_coding_2010$Text.x)]
human_coding_2010$Speech.x[is.na(human_coding_2010$Speech.x)]=human_coding_2010$Speech.y[is.na(human_coding_2010$Speech.x)]


human_coding_2010=full_join(human_coding_2010,coder_c_2010%>%select(Number,coding.c),by="Number")
human_coding_2010=rename(human_coding_2010,Speech_number=Speech.x)
human_coding_2010=rename(human_coding_2010,Text=Text.x)
names(human_coding_2010)


#############################################################
# Intercoderreliability
#############################################################
#To calculate Reliability, erase the cases in which three coders coded the statement. 
#This occured because of overlapping assignment
human_coding_for_irr=human_coding_2010
for(i in 1:nrow(human_coding_for_irr)){
  if(!is.na(sum(human_coding_for_irr[i,c("coding.a","coding.b","coding.c")]))){
    human_coding_for_irr$coding.c[i]=NA
  }
}
dat <- human_coding_for_irr[,c("coding.a","coding.b","coding.c")]
dat[dat==99]=NA
# How many cases in which all coders could not code the statement.
sum(apply(dat, 1, function(x) sum(is.na(x)))==3)
# Only keep statements that were coded by at least 2 coders
dat=dat[apply(dat, 1, function(x) sum(is.na(x)))<2,]
dat$coding.a[is.na(dat$coding.a)]=dat$coding.c[is.na(dat$coding.a)]
dat$coding.b[is.na(dat$coding.b)]=dat$coding.c[is.na(dat$coding.b)]
dat=dat[,c("coding.a","coding.b")]

kappa2(dat, "squared") # treating scale as ordinal

icc(dat) # treating scale as continuous, result is very similar

#############
# Aggregate at paragraph level
#############

meta.2010.protocol=rename(meta.2010.protocol,Speech_number=Number)
# Add in metadata
human_coding_2010=left_join(human_coding_2010,meta.2010.protocol)
#change code 99 to NA
human_coding_2010$coding.a[human_coding_2010$coding.a==99]=NA
human_coding_2010$coding.b[human_coding_2010$coding.b==99]=NA
human_coding_2010$coding.c[human_coding_2010$coding.c==99]=NA
#only keep cases that were coded by at least one coder
human_coding_2010=human_coding_2010%>%filter(!is.na(coding.a)|!is.na(coding.b)|!is.na(coding.c))
human_coding_2010$Speech_number=as.character(human_coding_2010$Speech_number)
#Transfer into corpus for analysis.
human_coding_2010_corpus=corpus(human_coding_2010,text_field = "Text",docid_field = "Speech_number")
#Add in speech number
docvars(human_coding_2010_corpus,"Speech_number")=human_coding_2010$Speech_number
#Run Sentiment dictionary on the corpus
human_coding_2010_dfm=data.frame(dfm(human_coding_2010_corpus,dictionary = data_dictionary_LSD2015))
#Calculate Sentiment
human_coding_2010_dfm$sentiment=log((human_coding_2010_dfm$positive+0.5)/(human_coding_2010_dfm$negative+0.5))
human_coding_2010_dfm$tokens=ntoken(human_coding_2010_corpus)
#Add it back to the raw data
human_coding_2010=cbind(human_coding_2010,human_coding_2010_dfm)

# Take the mean coding as the Handcoded variable
human_coding_2010$Sentiment_handcoded=apply(human_coding_2010%>%select(coding.a,coding.b,coding.c),1,mean,na.rm=T)
# Test: How well does Sentiment correlate at the paragraph level that was coded?
cor(human_coding_2010%>%select(Sentiment_handcoded,sentiment))

#############################################################
# Aggregated at party level

human_coding_2010_group_human=aggregate(human_coding_2010$Sentiment_handcoded,by=list(human_coding_2010$Group),mean)

names(human_coding_2010_group_human)=c("Name","Sentiment_handcoded_grouplevel")
#Add in Sentiment from the translations as constructe din 1.4
groups_2010=left_join(groups_2010,human_coding_2010_group_human)

#Calculate all correlations between the different translations
cors.groups.2010=cor(groups_2010%>%select(4:(ncol(groups_2010))))

#create new dataframe with the correlation to handcoding and the language
cors.groups.data=data.frame(cor_to_handcoded=cors.groups.2010[,ncol(cors.groups.2010)],
                            dictionary=row.names(cors.groups.2010))
cors.groups.data=cors.groups.data%>%filter(dictionary!="Sentiment_handcoded_grouplevel")
cors.groups.data$dictionary
# create new, more meaningful labels

cors.groups.data$labels=c("Bulgarian Dictionary","Czech Dictionary",
                          "Danish Dictionary","German Dictionary","Greek Dictionary","English Dictionary","Spanish Dictionary",
                          "Estonian Dictionary","Finnish Dictionary","French Dictionary","Hungarian Dictionary","Italian Dictionary",
                          "Lithuanian Dictionary","Latvian Dictionary","Dutch Dictionary",
                          "Polish Dictionary","Portuguese Dictionary","Romanian Dictionary",
                          "Slovak Dictionary","Slovenian Dictionary","Swedish Dictionary",
                          "German Dictionary extended","French Dictionary extended",
                          "Bulgarian SentiWS","Czech SentiWS",
                          "Danish SentiWS","German SentiWS",
                          "Greek SentiWS","English SentiWS","Estonian SentiWS","Finnish SentiWS",
                          "French SentiWS","Hungarian SentiWS","Italian SentiWS","Latvian SentiWS","Dutch SentiWS","French LSD Dictionary")

#Plot correlations for LSD
cors.groups.plot.lsd=ggplot(cors.groups.data%>%filter(!grepl("WS",labels)),aes(x=cor_to_handcoded,y=reorder(labels,cor_to_handcoded)))+
  geom_point(size=5)+
  theme_bw()+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(size=1,colour="grey88",linetype="dashed"),
        axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14),legend.title=element_text(size=16),legend.position="bottom")+
  labs(x="Correlation to Handcoding",y="")+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_vline(xintercept=1,linetype = "longdash")
cors.groups.plot.lsd

#Plot correlations for SentiWS
cors.groups.plot.sentiws=ggplot(cors.groups.data%>%filter(grepl("WS",labels)),aes(x=cor_to_handcoded,y=reorder(labels,cor_to_handcoded)))+
  geom_point(size=5)+
  theme_bw()+
  scale_shape_manual(values=c(16))+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(size=1,colour="grey88",linetype="dashed"),
        axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14),legend.title=element_text(size=16),legend.position="bottom")+
  labs(x="Correlation to Handcoding",y="")+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_vline(xintercept=1,linetype = "longdash")
cors.groups.plot.sentiws

#######################
# Plot Positions for Handcoding and English Lexicoder for the Groups
#bring all on the same scale
groups_2010$Sentiment_handcoded_grouplevel_norm=groups_2010$Sentiment_handcoded_grouplevel-3
groups_2010$Name
# Add correct group names
groups_2010$labels=c("Alliance of Liberals and Democrats for Europe (ALDE)",
                     "Commission",
                     "Council Speaker",
                     "European Conservatives and Reformists (ECR)",
                     "Europe of Freedom and Democracy (EFD)",
                     "European United Left-Nordic Green Left (GUE-NGL)",
                     "Non-Attached Members",
                     "European People's Party (EPP)",
                     "Progressive Alliance of Socialists and Democrats (S&D)",
                     "Greens-European Free Alliance (Greens-EFA)")
#scale everything
groups_2010$handcoded_scaled=scale(groups_2010$Sentiment_handcoded_grouplevel_norm,center = TRUE,scale = TRUE)
groups_2010$english_scaled=scale(groups_2010$Sentiment_en,center = TRUE,scale = TRUE)
#Transfer to long data frame
groups.for.plot=data.frame(Translation=c(rep("Handcoded",10),rep("English Dictionary",10)),
                           Correlation=c(groups_2010$handcoded_scaled,groups_2010$english_scaled),
                           labels=rep(groups_2010$labels,2),stringsAsFactors = F)

#groups.for.plot$labels=as.factor(groups.for.plot$labels)

pointplot_sotu_senti_2010=ggplot(groups.for.plot,aes(x=Correlation,y=reorder(labels,rep(groups.for.plot$Correlation[groups.for.plot$Translation=="Handcoded"],2)),colour=Translation,shape=Translation))+
  geom_point(size=6)+
  theme_bw()+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(size=1,colour="grey88",linetype="dashed"),
        axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14),legend.title=element_blank(),legend.position="bottom")+
  labs(x="Sentiment",y="")+
  scale_shape_manual(values=c(16,18))+
  scale_colour_manual(values=c("black","red"))
pointplot_sotu_senti_2010



###############
# For Appendix: Sentiment by Heads of Parties

# Sentiment by speaker 2010 English
#
senti_en_for_heads=data.frame(dfm(corpus_subset(protocol.2010.en,Group!="President"),groups="Name",
                                 remove_punct = TRUE,remove_numbers=TRUE, dictionary = extendeddict_en))
senti_en_for_heads$Sentiment_en=log((senti_en_for_heads$pos+0.5)/(senti_en_for_heads$neg+0.5))
senti_en_for_heads$Sentiment_sd=(1/(senti_en_for_heads$pos+0.5))+(1/(senti_en_for_heads$neg+0.5))
senti_en_for_heads$upper=senti_en_for_heads$Sentiment_en+1.96*senti_en_for_heads$Sentiment_sd
senti_en_for_heads$lower=senti_en_for_heads$Sentiment_en-1.96*senti_en_for_heads$Sentiment_sd
senti_en_for_heads=rename(senti_en_for_heads,Name=document)
senti_en_for_heads=left_join(senti_en_for_heads,meta.2010.unique)
kwic(corpus_subset(protocol.2010.en,Name=="Joseph Daul"),pattern = extendeddict_en$pos,window=8)

senti_en_for_heads=senti_en_for_heads[c(1,8,11,14,15,16,21,22,25,26),]
senti_en_for_heads$Name=c("Andreas Mölzer, (NI)","Daniel Cohn-Bendit (Verts/ALE)","Guy Verhofstadt (ALDE)","José Manuel Barroso (Commission)",
                          "Joseph Daul (EPP)","Lothar Bisky (GUE-NGL)","Martin Schulz (SD)","Michal Tomasz Kaminski (ECR)","Nigel Farage (EFD)",
                          "Olivier Chastel (Council)")
pointplot_sotu_senti_2010=ggplot(senti_en_for_heads,aes(xmin=lower,xmax=upper,x=Sentiment_en,y=reorder(Name,Sentiment_en)))+
  geom_errorbarh(height=0,size=1)+
  geom_point(size=6)+
  theme_bw()+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(size=1,colour="grey88",linetype="dashed"),
        axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14),legend.title=element_blank(),legend.position="bottom")+
  labs(x="Sentiment",y="")
pointplot_sotu_senti_2010


#############################
# For Appendix: Extract found words from some speeches

extract_found_words=function(X,Y){
  comp_toks <- tokens_compound(tokens(paste0(X,"$documents$texts")), phrase(Y))
  a=data.frame(table(unlist(tokens_select(comp_toks, Y))))
  a=a[order(a$Freq,decreasing = T),]
  return(a)
}

extract_found_words(protocol.2010.en[1],extendeddict_en$pos)
extract_found_words(protocol.2010.en[1],extendeddict_en$neg)
extract_found_words(protocol.2010.en[7],extendeddict_en$pos)
extract_found_words(protocol.2010.en[7],extendeddict_en$neg)

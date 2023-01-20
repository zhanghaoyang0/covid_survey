
A COVID-19 survey in China
=============================================
In Dec 2022, the COVID-19 restriction was end.

We conduct this 3-weeks on-line survey to measure the characteristics to measure the characteristics of COVID-19 patients in China. 

We investigate: 

Symptom spectrum of COVID-19:

.. image:: fig1.png
   :width: 600

Cluster of COVID-19 symptoms:

.. image:: fig2.png
   :width: 600

Regional distribution of COVID-19 syndromes:

.. image:: fig3.png
   :width: 600

Regional distribution of COVID-19 symptoms:

.. image:: fig4.png
   :width: 600


And, measure the association between COVID-19 symptoms and population characteristics, vaccination, and medication.

Questionnaire Data
=======================
The questionnaire was open for around 3 weeks, from 22th Dec 2022 to 17th Jan 2023.

552 questionnaires were collected.  

The data is in Chinese. Please see our code about how to clean this data.  


You can download the data at `here <https://github.com/zhanghaoyang0/covid_survey/raw/master/data/covid_survey.xlsx>`_.

Analysis code: preparation
=======================
We used R 4.0.3 for analysis.

Load packages:

.. code-block:: python

   libs = c('openxlsx', 'stringr', 'dplyr', 'stringi', 'R.utils', 'ggplot2', 'ggpubr', 'ggsci', 'mapchina', 'sf', 'data.table')
   lapply(libs, require, character.only = TRUE) 
   options(stringsAsFactors=F)
   sf::sf_use_s2(FALSE)
   

Define functions:

.. code-block:: python

   # calculate proportion of a given var for all, male, female participants
   # e.g, 
   # temp = data.frame(sex=c('Male', 'Female'), var=c(1, 0))
   # get_prop(temp, 'var')
   get_prop = function(df, var){
      for (sex1 in c('Male|Female', 'Male', 'Female')){
         df1 = df%>%filter(grepl(sex1, sex))
         tab = table(df1[,var])
         frq = data.frame(var=names(tab), n=as.numeric(tab))
         frq = frq%>%mutate(n=paste0(n,'(', sprintf('%.2f', n*100/sum(tab)),'%)'))
         print(paste0('distribution of ', ifelse(sex1=='Male|Female', 'all', tolower(sex1)), ' participants in ', var, ':'))
         print(frq)
      }
   }
   # convert character vector to numeric 
   # e.g, df = df%>%mutate_if(is_numeric,as.numeric)
   is_numeric <- function(x) {
      !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
   }


Analysis code: data cleaning and description
=======================
We conducted data cleaning by translating Chinese to English, combining groups with small samples, etc.
Description was also performed.

Load data and clean:

.. code-block:: python

   ## questionaire data
   df = read.xlsx('data/covid_survey_20230112.xlsx', sheet=1) # questionaire
   names(df) = gsub('/|，|？|“|”', '', names(df)) # remove Chinese punctuations
   names(df) = gsub('?', '', names(df), fixed=T) 
   names(df) = str_replace(names(df), '在感染后是否有出现以下[\U4E00-\U9FFF\U3000-\U303F]+症状:', '')
   names(df) = str_replace(names(df), '[（][\U4E00-\U9FFF\U3000-\U303F|1-9]+[）]', '')
   ## drop unused items and atypical symptons
   drop_cols = c('提交时间', '答题时间', '喉咙有刀割感', '吞咽时疼痛', '喉咙嘶哑', '喉咙干痒', '性欲减退', '生理期异常', '肾脏部位疼痛', '流泪', '打喷嚏')
   df[,drop_cols] = NULL # drop atypical symptons
   names(df)
   ## replace chinese with english
   dict1 = read.xlsx('data/covid_survey_20230112.xlsx', sheet=2)
   dict2 = read.xlsx('data/covid_survey_20230112.xlsx', sheet=3)
   dict = rbind(dict1, dict2[,c(1,4)])
   print('chinese items to english:')
   print(head(dict))
   for (i in 1:nrow(dict)){
      names(df)[names(df)==dict[i, 1]] = dict[i, 2]
   }
   

Age and sex:

.. code-block:: python

   df = df%>%mutate(age=gsub('岁', '', age))%>%
      mutate(age=ifelse(age%in%c('41-50', '51-60', '61-70'), '>40', age))%>%
      mutate(age=ifelse(age%in%c('12-18',  '18-24', '6-12', '3-6'), '<24', age))%>%
      mutate(age=factor(age, levels=c('<24', '24-30', '31-40', '>40')))
   df = df%>%mutate(sex=factor(ifelse(sex=='女','Female', 'Male'), levels=c('Female', 'Male')))
   table(df$sex)
   get_prop(df, 'age')


Disease duration:

.. code-block:: python

   # assume interval is left closed and right open, modify to reduce confusing
   df$infect_duration[df$infect_duration=='3～5天'] = '3~4天' 
   df$infect_duration[df$infect_duration=='5~7天'] = '5~6天'
   df$infect_duration[df$infect_duration=='7~10天'] = '7~9天'

   df = df%>%mutate(infect_duration=ifelse(infect_duration%in%c('7~9天', '10天以上'), '>7 day', infect_duration))%>%
      mutate(infect_duration=ifelse(infect_duration%in%c('', '小于3天'), '<3 day', infect_duration))%>%
      mutate(infect_duration=gsub('天', ' day', infect_duration))%>%
      mutate(infect_duration=gsub('~', '-', infect_duration))%>%
      mutate(infect_duration=factor(infect_duration, levels=c('<3 day', '3-4 day', '5-6 day', '>7 day')))
   get_prop(df, 'infect_duration')
   # trim fever_duration
   df$fever_duration = sapply(df$fever_duration, function(x){strsplit(x, '[(]')[[1]][1]})
   df = df%>%mutate(fever_duration=ifelse(is.na(fever_duration), 'no reply', fever_duration))%>%
      mutate(fever_duration=gsub('天', ' day', fever_duration))%>%
      mutate(fever_duration=ifelse(fever_duration%in%c('1 day', '<1 day'), '≤1 day', fever_duration))%>%
      mutate(fever_duration=factor(fever_duration, levels=c('no reply', '≤1 day', '2 day', '3 day', '>3 day')))
   get_prop(df, 'fever_duration')


Infect route

.. code-block:: python

df = df%>%mutate(
    infectway_entertainment=factor(as.numeric(grepl('消费场所', infect_way))), 
    infectway_work=factor(as.numeric(grepl('工作场所', infect_way))), 
    infectway_family=factor(as.numeric(grepl('在家被家人传染', infect_way))), 
    infectway_traffic=factor(as.numeric(grepl('公共交通', infect_way))), 
    infectway_hosp=factor(as.numeric(grepl('医疗场所', infect_way))))
for (i in c('infectway_entertainment', 'infectway_work', 'infectway_family', 'infectway_traffic', 'infectway_hosp')){
    print(i)
    get_prop(df, i)
}



Vaccination:

.. code-block:: python
   df[df$how_long_lastvac=='', 'n_vac'] = '0' # if a person report n_vac but not how_long_lastvac, treat n_vac as NA
   df = df%>%mutate(n_vac=ifelse(n_vac%in%c(3, 4), '≥3', n_vac))%>%
      mutate(n_vac=factor(n_vac, levels=c('0', '1', '2', '≥3')))

   df = df%>%mutate(how_long_lastvac=ifelse(how_long_lastvac=='', 'no_vac', how_long_lastvac))%>%
      mutate(how_long_lastvac=gsub('个月', ' month', how_long_lastvac))%>%
      mutate(how_long_lastvac=ifelse(how_long_lastvac%in%c('<3 month', '3-6 month'), '<6 month', how_long_lastvac))%>%
      mutate(how_long_lastvac=factor(how_long_lastvac, levels=c('no_vac', '<6 month', '6-12 month', '>12 month')))

   get_prop(df, 'n_vac')
   get_prop(df, 'how_long_lastvac')


Medication:

.. code-block:: python

# the 'drug_use' have been grouped to four groups, by hand
drugs = c('ibuprofen_use', 'acetaminophen_use', 'chnmed_usd')
df[, drugs][is.na(df[, drugs])] = 0
for (drug in drugs){
    print(drug)
    get_prop(df, drug)
    df[,drug] = as.factor(df[,drug])
}


Calculate syndrome score with symptom score, normalize to 0-1:

.. code-block:: python

   ## calculate syndrome score with symptom score, normalize to 0-1
   syndromes = unique(dict2$syndrome)
   symptoms = list()
   for (i in syndromes){
      symptoms[[i]] = dict2%>%filter(syndrome==i)%>%pull(item_eng)
   }
   print(symptoms)
   print(sum(is.na(df[,unlist(symptoms)]))) # number of NA
   for (syndrome in syndromes){
      score = rowSums(df[,symptoms[[syndrome]]])/length(symptoms[[syndrome]])/3 # normalize to 0-1
      df[,paste0(syndrome, '_score')] = score
   }


Region:

.. code-block:: python

   ## region
   df$region = gsub('维吾尔|壮族', '', df$region)
   regions = c()
   for (i in 1:nrow(df)){
      item = df[i, 'region']
      item1 = strsplit(item, '自治区|自治州|特别行政区|省|市')[[1]][1]
      regions = c(regions, item1)
   }
   df$region = regions
   # replace chn with pinyin
   data(china)
   china = china%>%mutate(region=gsub('省|市|回族|壮族|维吾尔|特别行政区|自治区', '', Name_Province))
   china = china%>%group_by(region)%>%dplyr::summarise(geometry=st_union(geometry))
   replace = data.frame(region=china$region, 
   region1 = c('Shanghai', 'Yunnan', 'Neimenggu', 'Beijing', 'Taiwan', 'Jilin', 'Sichuan', 'Tianji', 
   'Ningxia', 'Anhui', 'Shandong', 'Shānxi', 'Guangdong', 'Guangxi', 'Xinjiang', 'Jiangsu', 'Jiangxi', 'Hebei', 
   'Henan', 'Zhejiang', 'Hainan', 'Hubei', 'Hunan', 'Macau', 'Gansu', 'Fujian', 'Tibet', 'Guizhou', 'Liaoning', 
   'Chongqing', 'Shǎnxi', 'Qinghai', 'Hong Kong', 'Heilongjiang'))
   china = china%>%merge(replace, 'region')%>%select(-region)%>%rename(region=region1)
   df = df%>%merge(replace, 'region')%>%select(-region)%>%rename(region=region1)
   print(table(df$region))
   # sample size for each region
   tab = table(df$region)
   tab = data.frame(cbind(names(tab), tab))
   pop_tab = tab%>%rename(n=tab, region=V1)%>%mutate(n=as.numeric(n))%>%arrange(n)
   print(pop_tab)



Comments and feedbacks
=======================
Feel free to contact me via zhanghaoyang0@hotmail.com

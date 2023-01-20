
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

Comments and feedbacks
=======================
Feel free to contact me via zhanghaoyang0@hotmail.com

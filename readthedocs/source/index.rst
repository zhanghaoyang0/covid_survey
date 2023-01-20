
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

Analysis code
=======================
We used R 4.0.3 for analysis.

Load packages:

.. code-block:: python

   libs = c('openxlsx', 'stringr', 'dplyr', 'stringi', 'R.utils', 'ggplot2', 'ggpubr', 'ggsci', 'mapchina', 'sf', 'data.table')
   lapply(libs, require, character.only = TRUE) 
   options(stringsAsFactors=F)
   sf::sf_use_s2(FALSE)
   

Defind functions:

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



Comments and feedbacks
=======================
Feel free to contact me via zhanghaoyang0@hotmail.com

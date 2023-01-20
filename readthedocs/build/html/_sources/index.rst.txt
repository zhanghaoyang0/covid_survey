
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
We used R 4.0.3 for analysis, with follow packages and functions1. 

.. code-block:: python
   
   libs = c('openxlsx', 'stringr', 'dplyr', 'stringi', 'R.utils', 'ggplot2', 'ggpubr', 'ggsci', 'mapchina', 'sf', 'data.table')
 




Comments and feedbacks
=======================
Feel free to contact me via zhanghaoyang0@hotmail.com

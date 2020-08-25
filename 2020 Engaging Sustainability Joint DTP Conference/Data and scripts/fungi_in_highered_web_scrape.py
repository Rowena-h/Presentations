# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 18:47:25 2020

@author: Rowena

Rough and ready web scraping script to determine the number of times fungi are mentioned in UK biology undergraduate modules at Russell Group universities and the number of courses on specifically fungi (relative to the other eukaryote kingdoms of animals and plants)
"""

from googlesearch import search
import requests
import re
import pandas as pd
from bs4 import BeautifulSoup

##Webscrape of eukaryote kingdom mentions in Russell university biology modules##

#Make list of Russell Group universities
russell_group_list = ["birmingham", "bristol", "cambridge", "cardiff", "durham", "edinburgh", "exeter", "glasgow", "imperial", "kings", "leeds", "liverpool", "lse", "manchester", "newcastle", "nottingham", "oxford", "queen mary", "belfast", "sheffield", "southampton", "ucl", "warwick", "york"]

#Make empty variables to add results to
russell_links = []
biology_df = []

#For each uni (and its index) in the Russell group list...
for  idx, uni in enumerate([s + " biology undergraduate modules" for s in russell_group_list]):
        
    #Find the top google result for biology modules
    for url in search(query=uni,
                      tld = 'com',
                      lang = 'en',
                      num = 1,
                      start = 0,
                      stop = 1,
                      pause = 2.0):
        
        #Add the link to the results list (in case you want to check manually for sensible links)
        russell_links.append(url)
        #Get the page details
        page = requests.get(url).text
        
        #For each of the eukaryote kingdoms...
        for euk in ["animal", "plant", "fungi"]:
            #Add the count of uni modules to the results list
            biology_df.append({'uni' : russell_group_list[idx],
                           'eukaryote' : euk, 
                           'count' : len(re.findall(euk, page))})

#Convert the module count results list to a dataframe
biology_df = pd.DataFrame(biology_df, columns=['uni', 'eukaryote', 'count'])

#Export the dataframe to csv
biology_df.to_csv('D:\\Documents\\Kew\\Outreach\\Understudied fungi\\uk_biology_modules.csv', index=False)


##Webscrape of eukaryote kingdom mentions in courses listed on whatuni.com##

#Make empty variable to add results to
courses_df = []

#For each of the eukaryote kingdoms...
for euk in ["animal", "plant", "fungi"]:
    #Reset empty results variable
    results = []
    #Search for the kingdom on the whatuni website
    url = 'https://www.whatuni.com/degree-courses/search?q=' + euk
    page = requests.get(url)
    #Parse the results with BeautifulSoup
    soup = BeautifulSoup(page.content, 'html.parser')
    #Search for the html class which contains number of hits
    results = soup.find(class_="result-head respar2")
    #If the class exists (i.e. successful search)...
    if results:
        #Add the number of course hits to the results list
        courses_df.append({'eukaryote' : euk,
                           'count' : results.text.split('courses')[0].split()[-1]})
        #If the class doesn't exist (i.e. unsuccessful search)...
    else:
        #Add 0 course hits to the results list
        courses_df.append({'eukaryote' : euk,
                           'count' : 0})
    
#Convert the course count results list to a dataframe
courses_df = pd.DataFrame(courses_df, columns=['eukaryote', 'count'])

#Export the dataframe to csv
courses_df.to_csv('D:\\Documents\\Kew\\Outreach\\Understudied fungi\\whatuni_courses.csv', index=False)
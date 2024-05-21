
## Importing all the required packages
from bs4 import BeautifulSoup
import requests
import pandas as pd
import numpy as np
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time, os, shutil
from send2trash import send2trash

def clean_crude(csv):
    no_blank = [] 
    for i in csv:
        for j in i:
            if j != '':
                no_blank.append(j)
        new_data_split = []
    for i in no_blank:
        new_line = pd.DataFrame(i.split(','))
        new_data_split.append(new_line)
    season_data = pd.concat(new_data_split, ignore_index = True, axis = 1).T
    header = season_data.iloc[2]
    season_data = season_data[3:]
    season_data.columns = header
    return season_data

def raw_bbref_pit(year):
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.baseball-reference.com/leagues/majors/"+str(year)+"-standard-pitching.shtml"
    driver.get(url)
    time.sleep(5)
    driver.execute_script("window.scrollTo(0, 1500);")
    element = driver.find_element('xpath','//*[@id="players_standard_pitching_sh"]/div/ul/li[1]')
    driver.execute_script("arguments[0].click();", element)
    time.sleep(5)
    element2 = driver.find_element('xpath','//*[@id="players_standard_pitching_sh"]/div/ul/li[1]/div/ul/li[3]/button')
    driver.execute_script("arguments[0].click();", element2)
    soup = BeautifulSoup(driver.page_source, 'html.parser')
    crude = soup.find('pre', id = 'csv_players_standard_pitching').text.split("\n")
    
    soup_list = []
    soup_list.append(crude)
    clean = clean_crude(soup_list)
    return clean

def raw_bbref_bat(year):
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.baseball-reference.com/leagues/majors/"+str(year)+"-standard-batting.shtml"
    driver.get(url)
    time.sleep(5)
    driver.execute_script("window.scrollTo(0, 1500);")
    element = driver.find_element('xpath','//*[@id="players_standard_batting_sh"]/div/ul/li[1]')
    driver.execute_script("arguments[0].click();", element)
    time.sleep(5)
    element2 = driver.find_element('xpath','//*[@id="players_standard_batting_sh"]/div/ul/li[1]/div/ul/li[3]/button')
    driver.execute_script("arguments[0].click();", element2)
    soup = BeautifulSoup(driver.page_source, 'html.parser')
    crude = soup.find('pre', id = 'csv_players_standard_batting').text.split("\n")
    
    soup_list = []
    soup_list.append(crude)
    clean = clean_crude(soup_list)
    return clean

def raw_fgraphs_pit_nl(year):
    ## Initialising the chrome webdriver
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=nl&qual=0&type=c,7,13,59&season="+str(year)+"&month=0&season1="+str(year)+"&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate="

    ## Opening the url in a seperate chrome window
    driver.get(url)

    ## Attaching a sleep time to selenium so that the page loads completely
    time.sleep(3)

    ## Find the element on the page with the link text function of Selenium
    element = driver.find_element("link text",'Export Data')
    driver.execute_script("arguments[0].click();", element)

    ## Again waiting for 5 seconds so that the download is complete
    time.sleep(3)

    ## Closing the browser window
    driver.quit()
    
    data = pd.read_csv("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    send2trash("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    return data

    ## Moving the file from downloads folder to the Project Folder
    ##shutil.move('/Users/shenyan/Downloads/FanGraphs Leaderboard.csv','/Users/shenyan/Desktop/expansion/pitching'+str(year)+'_f_NL.csv')

def raw_fgraphs_pit_al(year):
    ## Initialising the chrome webdriver
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=al&qual=0&type=c,7,13,59&season="+str(year)+"&month=0&season1="+str(year)+"&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate="

    ## Opening the url in a seperate chrome window
    driver.get(url)

    ## Attaching a sleep time to selenium so that the page loads completely
    time.sleep(3)

    ## Find the element on the page with the link text function of Selenium
    element = driver.find_element("link text",'Export Data')
    driver.execute_script("arguments[0].click();", element)

    ## Again waiting for 5 seconds so that the download is complete
    time.sleep(3)

    ## Closing the browser window
    driver.quit()
    
    data = pd.read_csv("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    send2trash("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    return data

    ## Moving the file from downloads folder to the Project Folder
    ##shutil.move('/Users/shenyan/Downloads/FanGraphs Leaderboard.csv','/Users/shenyan/Desktop/expansion/pitching'+str(year)+'_f_AL.csv')

def raw_fgraphs_bat_al(year):
    ## Initialising the chrome webdriver
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=al&qual=0&type=c,4,5,6,58&season="+str(year)+"&month=0&season1="+str(year)+"&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate="

    ## Opening the url in a seperate chrome window
    driver.get(url)

    ## Attaching a sleep time to selenium so that the page loads completely
    time.sleep(3)

    ## Find the element on the page with the link text function of Selenium
    element = driver.find_element("link text",'Export Data')
    driver.execute_script("arguments[0].click();", element)

    ## Again waiting for 5 seconds so that the download is complete
    time.sleep(3)

    ## Closing the browser window
    driver.quit()
    
    data = pd.read_csv("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    send2trash("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    return data

    ## Moving the file from downloads folder to the Project Folder
    ##shutil.move('/Users/shenyan/Downloads/FanGraphs Leaderboard.csv','/Users/shenyan/Desktop/expansion/batting'+str(year)+'_f_AL.csv')

def raw_fgraphs_bat_nl(year):
    ## Initialising the chrome webdriver
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=nl&qual=0&type=c,4,5,6,58&season="+str(year)+"&month=0&season1="+str(year)+"&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate="

    ## Opening the url in a seperate chrome window
    driver.get(url)

    ## Attaching a sleep time to selenium so that the page loads completely
    time.sleep(3)

    ## Find the element on the page with the link text function of Selenium
    element = driver.find_element("link text",'Export Data')
    driver.execute_script("arguments[0].click();", element)

    ## Again waiting for 5 seconds so that the download is complete
    time.sleep(3)

    ## Closing the browser window
    driver.quit()
    
    data = pd.read_csv("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    send2trash("/Users/yanshen/Downloads/FanGraphs Leaderboard.csv")
    
    return data

    ## Moving the file from downloads folder to the Project Folder
    ##shutil.move('/Users/shenyan/Downloads/FanGraphs Leaderboard.csv','/Users/shenyan/Desktop/expansion/batting'+str(year)+'_f_NL.csv')

def raw_bioinfo(playerid):
    chromedriver = "/Users/yanshen/Documents/GitHub/full-house-update/chromedriver"
    #os.environ["webdriver.chrome.driver"] = chromedriver
    driver = webdriver.Chrome(chromedriver)
    #cService = webdriver.ChromeService(executable_path='/Users/yanshen/Desktop/PhD_UIUC/pythonlearning/chromedriver')
    #driver = webdriver.Chrome(service = cService)
    
    ## Attaching the yeear number in the url to pull the required url
    url = "https://www.baseball-reference.com/players/"+str(playerid[0])+"/"+str(playerid)+".shtml" 
    driver = webdriver.Chrome(chromedriver) 
    driver.get(url) 
    time.sleep(5) 
    soup = BeautifulSoup(driver.page_source, 'html.parser') 
    crude = soup.find(id = 'meta').text.split("\n") 
    while("" in crude): 
      crude.remove("") 
    crude.append(playerid) 
    df = pd.DataFrame(crude) 
    
    return df



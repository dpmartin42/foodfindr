'''
Author: Dan Martin
Date: 7/14/15
Title: Read in .csv created in R as a python DataFrame and create a 
       SQL database for the flask app
'''

import pandas as pd
import MySQLdb

def pandas_to_sql(df, database, table):
    db = MySQLdb.connect(host = 'localhost',
                         user = 'root',
                         passwd = "")     

    db.query('CREATE DATABASE IF NOT EXISTS ' + database + ';')
    db.query('USE ' + database + ';')                                                                                                                                                                                    
    the_data.to_sql(name = table,
                    con = db,
                    flavor = 'mysql',
                    if_exists = 'replace')                                                                                                                                                     
    db.close()
    
    return

if __name__ == "__main__":
    
    # Read in data
    the_data = pd.read_csv("food_data.csv")

    the_data.columns = ['name', 'addresses', 'price', 'health_score']
    
    pandas_to_sql(the_data, 'food_db', 'food_tb')
                                


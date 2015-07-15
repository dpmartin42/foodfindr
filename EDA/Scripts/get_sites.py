'''
import urllib2

data = urllib2.urlopen(target_url) # it's a file like object and works just like a file
for line in data: # files are iterable
    print line


r = urllib2.urlopen('http://boston.menupages.com/restaurants/a4-truck/menu').readlines()
#soup = BeautifulSoup(r)
#print type(soup)


# Potential issues:
# Need to check if there are 100 places. If so, go to the next page

'''

from bs4 import BeautifulSoup
import urllib2

url = 'http://www.fatsecret.com/calories-nutrition/search?q=chicken'
page = urllib2.urlopen(url)
soup = BeautifulSoup(page.read())

print soup
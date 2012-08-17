#!/usr/bin/python

import urllib2
import simplejson
import sys

def get_json(url):
    try:
	r = urllib2.Request(url)
	opener = urllib2.build_opener()
	f = opener.open(r)
	json = simplejson.load(f)
    except:
	return None
    else:
	return json

companies = get_json('http://api.crunchbase.com/v/1/companies.js')

if not companies:
    sys.exit()

from pymongo import Connection
connection = Connection()
db = connection.crunch
company = db.company

print len(companies)

for c in companies:
    r = get_json('http://api.crunchbase.com/v/1/company/' + c['permalink'] + '.js')
    if r:
	print c['permalink']
	company.insert(r)

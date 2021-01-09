#!/usr/bin/env python

import urllib
import xml.dom.minidom

try:

    weather_url = "http://weather.gov/data/current_obs/KANE.xml"
    resp_fh = urllib.urlopen(weather_url)
    resp = resp_fh.read()
    dom = xml.dom.minidom.parseString(resp)
    weather = dom.getElementsByTagName("weather")[0].childNodes[0].data
    temp_f = dom.getElementsByTagName("temp_f")[0].childNodes[0].data
    windchill_f = dom.getElementsByTagName("windchill_f")[0].childNodes[0].data

    print "Weather: %s %sF" % (weather, temp_f),

    # sometimes windchill is not applicable
    try:
	    print "Windchill %dF" % int(windchill_f)
    except:
	    print

except:

    print "Weather: Not Available"

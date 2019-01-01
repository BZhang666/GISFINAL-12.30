import googlemaps
import csv
import time

gmaps = googlemaps.Client(key='YOUR KEY') #I deleted my API key here, you can enter your Key if you want to reuse this code

with open('Siteinfomation.csv', mode='r') as locationsFile:
    with open('location.csv', mode='w') as geocodedLocationsFile:
        locationsReader = csv.reader(locationsFile)
        locationsWriter = csv.writer(geocodedLocationsFile)
        locationsWriter.writerow(['location', 'lat', 'lng'])

        # skip headers
        next(locationsReader)
      
        for location in locationsReader:
            locationAddress = location[0]+", London"

            try:
                geocodedResult = gmaps.geocode(locationAddress)
                geocodedResult = geocodedResult[0]
                if (geocodedResult):
                    locationsWriter.writerow([
                       locationAddress,
                       geocodedResult.get('geometry').get('location').get('lat'),
                       geocodedResult.get('geometry').get('location').get('lng'),
                ])

                time.sleep(0.05)  #set a sleep time to satisfy the requirement of google API
            except IndexError:
              locationsWriter.writerow([
                    locationAddress,
                    "N/A"
                    "N/A"
                ])

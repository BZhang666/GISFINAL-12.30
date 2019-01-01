import googlemaps
import csv
import time

gmaps = googlemaps.Client(key='AIzaSyBbZ0orHv_hda3djm-ZTefFzeHsKfaCkSQ')

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

                time.sleep(0.05)
            except IndexError:
              locationsWriter.writerow([
                    locationAddress,
                    "N/A"
                    "N/A"
                ])
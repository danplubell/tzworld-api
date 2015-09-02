#tzworld-api
This package contains an api that can be used to find the time zone for a location by the longitude and the latitude.

The data is from the tz_world data created by Eric Muller.  The last update of the data was made on November 26, 2013.

You can find the data here: [http://efele.net/maps/tz/world/]()

**API**  
Use the following method to retrieve the Olson timezone name:

`Data.TZworld.Api.findTZByLoc::Double->Double->IO (Either String (Maybe String)`

If there is an error then a message is returned as a Left String value.

Otherwise a Right (Maybe String) value is returned.
In this case Nothing is returned if the timezone wasn't found in the database.
Otherwise a Just String value is returned that includes the Olson timezone name.

**Limitations**  
This api does not return time zones for territorial waters. 

**Dependencies**  
This api depends on a location installation of SQLite3.  It does not install SQLite3.


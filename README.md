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

#License
The MIT License (MIT)

Copyright (c) 2015 Dan Plubell <danplubell@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

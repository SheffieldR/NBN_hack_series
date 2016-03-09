# Notes on how determined hectads that intersect Sheffield local authority boundary

1. Created a project in [QGIS][qgis] that uses the [EPSG:27700][bng] (British National Grid) spatial reference system. 
2. Imported a [shapefile][shapefile-wikipedia] of [local authority district boundaries][district-bounds] (dated Dec 2012).  Each entity in the shapefile is a polygon.
3. Imported a [Shapefile of UK hectads][hectad-grids].  Again, each entity is a polygon.
4. Exported the local authority boundary for Sheffield (where `LAD12NM` attribute is `Sheffield`) as a single-polygon shapefile ([`Sheffield_Dec_2015_boundary.shp`](Sheffield_Dec_2015_boundary.shp)).
5. Manually noted down the names of all hectads (10km x 10km squares) that overlapped at all with the Sheffield local authority boundary ([`hectads_intersecting_sheffield_city_boundary.csv`](hectads_intersecting_sheffield_city_boundary.csv)).

### To do

1. Extract a superset of the relevant data using the NBN API and `hectads_intersecting_sheffield_city_boundary.csv` 
2. Prune the resulting dataframe to just relevant data using `Sheffield_Dec_2015_boundary.shp` (possibly also using a [GIS buffer][gis-buffer] in the spatial query to account for survey size).  Within the spatial query it may or may not be useful to convert British National Grid references (with alpha prefixes; see `location` column in NBN data) to easting and northing pairs (of floats) using `epi.convgrid(os.refs)` from the `epiR` package.

[qgis]: http://www.qgis.org/
[bng]: http://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/
[district-bounds]: https://data.gov.uk/dataset/local-authority-district-gb-dec-2012-boundaries-full-extent
[hectad-grids]: https://github.com/charlesroper/OSGB_Grids
[shapefile-wikipedia]: https://en.wikipedia.org/wiki/Shapefile
[gis-buffer]: https://en.wikipedia.org/wiki/Buffer_(GIS)

# blackspot
[![docker automated](https://img.shields.io/docker/automated/blmoore/blackspot.svg)](https://hub.docker.com/r/blmoore/blackspot/)

Shiny app exploring Edinburgh traffic collision data. View from R with: 

```r
shiny::runGitHub("blmoore/blackspot")
```

Or use docker (view at http://localhost:3838):

```
docker run -dp 3838:3838 blmoore/blackspot
```

Note `analytics.js` contains my Google analytics tracking code, you'll want to comment this out (i.e. `includeScript("analytics.js")` in `ui.R`) or swap in your own code if you're deploying this app!

## Screenshots
### v0.4
![screenshot of shiny app](http://blm.io/images/blackspot_v04.png)

### v0.3
![screenshot of shiny app](http://blm.io/images/blackspot_v03.png)

## Credits

* Code: some shiny code (especially UI) adapted from [SuperZip](https://github.com/jcheng5/superzip).
* Data: Edinburgh open data [vehicle collisions](http://www.edinburghopendata.info/dataset/vehicle-collisions).

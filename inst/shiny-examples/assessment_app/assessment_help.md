Pre-eradication Assessment app user guide
-----------------------------------------

This shiny app is primarily to be used for undertaking an initial
assessment of a pest population prior to undertaking eradication
activities. This help file provides some guidance about the use of the
app including the types of monitoring data as well as the associated
models that can be used to undertake an initial assessment of a pest
population. Before using this app, the practitioner should ideally be
familiar with the material in the accompanying document
`Quantitative decision support for eradication - a primer`. In
particular, the practitioner should have read
`Spatial considerations: Extent, Management Zones and Sampling Units`.

This user guide also makes use of the example data provided in the
accompanying ZIP file. Eventually, the app will be able to automatically
load these examples directly but for now, please unzip these files into
a convenient location.

### Initial assessment of a pest population

An initial estimate of the size of the pest population will enable a
first estimate to be made of the likely costs associated with achieving
eradication. It will also enable an initial evaluation of different
monitoring techniques. Several techniques are available for estimating
population abundance based on repeated sampling of unmarked individuals,
plots, or resightings of marked individuals. The method chosen will
depend on both the type of monitoring device used to make observations
and the ease with which it can be deployed. In general, sampling a pest
population to obtain information on population size and/or distribution
can be divided into methods based on counts of individuals or presence
of sign and methods based on marked animals. The assessment app employs
methods based on counts of individuals and presence of sign within
sampling units. In general, monitoring data should be collected from
sampling units that are independent. By independent, we mean that an
individual counted (or recorded as present) on one sampling unit should
not be able to be recorded on another sampling unit. Some particular
models (e.g. `REST`) are tailored to particular types of sampling
methods such as remote infrared cameras and require ancillary
information to be collected. They also allow the relaxation of the
strict assumption of independence of sampling units required by the
other methods.

### Models

The basic models in the pre-eradication assessment app can be divided
into those that just require records of pest presence or absence from
sampling units (i.e. `Occupancy` and `Royle-Nichols` models) and those
that require counts of individuals to be recorded (i.e. `N-mixture` and
`REST`). These are briefly described below:

-   `Occupancy`
    -   This model implements the occupancy model of Mackenzie et
        al. (2006). It is primarily used to assess the area of the
        region occupied by the pest from presence/absence monitoring
        data. Occupancy estimates can also depend on habitat
        characteristics. Presence-absence data should be collected from
        sampling units that are independent.  
-   `Royle-Nichols`
    -   This model implements the occupancy model of Royle and Nichols
        (2003). However, this model has the occupancy rate being
        dependent on pest abundance and hence, can be used to provide an
        estimate of abundance in addition to occupancy. The extra
        parameter `K` can be provided here which is an upper limit on
        the likely abundance at each site. Larger values are preferred,
        but will increase the time to fit the model.
-   `N-mixture`
    -   This model estimates pest abundance from counts of individuals
        collected from independent sampling units using the `N-mixture`
        model of Royle (2004). As for the `Royal-Nichols` model The
        parameter `K` should also be provided.
-   `REST`
    -   This model estimates of pest density from encounters of
        individuals from camera traps using the “Random Encounter and
        Staying-Time” model of Nakashima et al (2018). In addition to
        recording the number of encounters at each camera, the “staying
        time” of each individual in front of the camera must also be
        recorded. The staying time can be estimated as the difference
        between the exit and entry times into the camera’s field of
        view. This model also requires an estimate of the effective
        detection area of the camera (i.e. area within which detection
        of an indiviudal is almost certain). This can be estimated using
        distance sampling techniques.

### Inputs

All models in the assessment app require inputs in particular formats

-   `Region boundary`
    -   The shapefiles that define the region of interest. Select all
        files (`.shp`, `.shp`, `.dbf`, `.prj` ) as a group. Shapefies
        should be projected, rather than in geographic format.
-   `Habitat raster`
    -   A `.tif` file(s) containing a map of habitat variable(s) that
        could be used predict spatially-varying abundance across the
        region. Multiple `.tif` files can be selected.
-   `Detector locations`
    -   A `csv` file containing coordinates of `M` monitoring devices in
        the same projection as `region boundary`. The first column
        should be the easting (x) coordinate and the second the northing
        (y).
-   `Detection histories`
    -   A `.csv` file containing a `M` x `J` matrix of detection or
        counts with observations for each monitoring device in rows and
        `J` repeat observations for each device (e.g. days) in columns.

In addition to the above, selecting the `REST` model exposes some
additional data that is needed by that model.

-   `Stay`
    -   A `.csv` file containing a single column with the residence time
        (i.e. seconds) that each encountered individual spent in front
        of the camera (i.e. the difference between exit and entry
        times).
-   `Censored`
    -   a `.csv` file containing a single column containing a variable
        indicating whether the exit time for each encountered individual
        was observed (`censored` = 1) or unobserved (`censored` = 0).
-   `active_hours`
    -   The number of hours within a 24 hour period that the target
        species is usually active for. Species that are active at any
        time of the day would have a value of 24 and species active only
        at night would have a value around 12.
-   `Area of camera viewshed`
    -   The area of the effective detection zone in front of a typical
        camera (m**<sup>2</sup>). This Can be estimated using distance
        sampling techniques. An approximate area can be calculated by

where `r` is the maximum radial distance from a camera that a species
would be detected with near certainty and *θ* is the coverage angle of
the camera (in degrees). For more details on estimating an effective
camera area, see Hofmeester et al. (2017).

-   `Viewshed area multiplier`
    -   The multiplier required to express density in units different
        from the units of `area of camera viewshed`. If
        `area of camera viewshed` is in m**<sup>2</sup> then to express
        pest density in hectares the `viewshed multiplier` should be set
        to 10,000. If density in km**<sup>2</sup> is required it should
        be set to 1,000,000.

Analysis
--------

Once the data for a particular model has been uploaded, pressing the
`fit model` button will run the selected model and print the output in
the “fitted model” pane. Outputs include the parameter estimates and a
prediction of the population size based on the monitored sites. Any
habitat covariates uploaded previously should also be available and can
be selected in the model if desired. The `Estimate density surface`
button displays a map of the predicted density across the region.

### References

Hofmeester, T. R., Rowcliffe, J. M., & Jansen, P. A. (2017). A simple
method for estimating the effective detection distance of camera traps.
Remote Sensing in Ecology and Conservation, 3(2), 81–89.

MacKenzie, D. I., Nichols, J. D., Royle, J. A., Pollock, K. H., Bailey,
L. L., & Hines, J. E. (2006). Occupancy estimation and modelling:
inferring patterns and dynamics of species occurrence. Academic Press

Royle, J Andrew, & Nichols, J. D. (2003). Estimating abundance from
repeated presence–absence data or point counts. Ecology, 84(3), 777–790

Royle, J.A. (2004) Generalized estimators of avian abundance from count
survey data. Animal Biodiversity and Conservation, 27, 375–386

Nakashima, Y., Fukasawa, K., & Samejima, H. (2018). Estimating animal
density without individual recognition using information derivable
exclusively from camera traps. Journal of Applied Ecology, 55(2),
735–744

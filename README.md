Demographic Data
================

The closest matches between deaths and resident population are infoshare
population As At December and Age at death.

But these are not exact matches as population is age at December and
deaths are age at time of death. So for someone 79 in December 2018 who
is no longer live to be 80 in December 2019, they could have died at age
79 or 80. But this is a systematic error- they didn’t die at 78, and the
degree of 79/80ishness is largely a combination of birthdate realtive to
available time to time (semi-random based on overall annual birthday
frequency) and risk periods within the year (precovid the distribution
of risk of death though the year is largely controlled by how bad the
winter is relative to the summer minima).

Also, this incompatibility of threshold periods is minimised with large
age aggregates because it is a issue with the border regions between
groups.

Technically, any border crossing data used will have a similar
systematic issue as it is age at date of crossing border. But long term
migration tends (as I recall) to occur more at the start of the year, so
stocastically is a better match to Age at end of December.

For ages above 0 (the first year also has birth input)

Ignoring migration, the expected population is:

Existing group + previous year aging into age range - max year of age
range aging out of age range - deaths within age rage. For single year
age ranges this is simplified as the whole current age range is expected
to age out over a year.

Expected = Population<sub>Y-1</sub> +
Population<sub>Agemin-1/Y-1</sub> - Population<sub>Agemax/Y-1</sub> -
Deaths<sub>Y0</sub> + t (wh\~re t is any threshold issues introduced by
the datasets age at death vs age at Dec 31st)

And actual population should be Expected + migration effects, so what
needs explaining by migration is

Migration = Actual - Expected(if ignoring migration)

Datasets:

via https://infoshare.stats.govt.nz

Population : Population Estimates - DPE : Estimated Resident Population
by Age and Sex (1991+) (Annual-Dec) / Estimate Type: As At / Population
Group: Select All / Observations: Select All / Time: Select All /
Download as csv (in my case DPE403905_20240110_124716_29.csv)

Population : Deaths - VSD : Deaths by age and sex (Annual-Dec) / Sex:
Total Both Sexes / Observations: Select All / Time: Select All /
Download as csv (in my case VSD349204_20240110_125331_51.csv)

``` r
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(knitr)
```

``` r
infopop <- read.csv("DPE403905_20240110_124716_29.csv",skip=3) |> 
  filter(!is.na(X0.Years)) |> 
  mutate(X. = as.numeric(X.)) |> 
  rename(Year = X.)
infodth <- read.csv("VSD349204_20240110_125331_51.csv", skip=2) |> filter(!is.na(Total.all.ages)) |> 
  mutate(X. = as.numeric(X.)) |> 
  rename(Year = X.)
```

To minimise threshold influence on an exploratory test from 2010 I am
checking the broad 15-64 and 65 plus categories:

For 2011 15-64 expected population, it is

2010 15-64 population + 2010 14 year old population (aged into) - 2010
64 year old population (aged out) - 2011 deaths 15-64 (died) + threshold
effects (expected to be minor over large groups)

So 2011 migration needs to explain

2011 15-64 actual population - 2011 15-64 expected population

For 2011 65+ population the 2010 65+ expected value is

2010 65+ population + 2010 64 year old population (aged into - 2011
deaths 65+ (died)

And the needing to be explained by migration is

2010 65+ actual - 2010 65+ expected

And so on for other years. For which an implementation of R code to
return the migration effect is:

``` r
dth2010 <- infodth |> filter(Year > 2009) |> 
  mutate(X15.64 = X15.19.years + X20.24.years + X25.29.years +
           X30.34.years + X40.44.years + X45.49.years + 
           X50.54.years + X55.59.years + X60.64.years,
         X65.plus = X65.69.years + X70.74.years +
           X75.79.years + X80.84.years + X85.89.years +
           X90.94.years + X95.99.years + X100.years.and.over) |> select(Year, X15.64, X65.plus)
colnames(dth2010) <- paste0("D", colnames(dth2010))
colnames(dth2010)[1] <- "Year"
pop2010 <- infopop |> filter(Year > 2009) |> 
  select(Year, X15.64.Years, X14.Years, X64.Years, X65.Years.and.Over)
colnames(pop2010) <- paste0("P", colnames(pop2010))
colnames(pop2010)[1] <- "Year"
combo <- pop2010 |> inner_join(dth2010, by = join_by(Year)) |>
  mutate(E15.64 = lag(PX15.64.Years) + lag(PX14.Years) - lag(PX64.Years) - DX15.64,
         M15.64 = PX15.64.Years - E15.64,
         E65.plus = lag(PX65.Years.and.Over) + 
           lag(PX64.Years) - DX65.plus,
         M65.plus = PX65.Years.and.Over - E65.plus)
combo |> kable()
```

| Year | PX15.64.Years | PX14.Years | PX64.Years | PX65.Years.and.Over | DX15.64 | DX65.plus |  E15.64 | M15.64 | E65.plus | M65.plus |
|-----:|--------------:|-----------:|-----------:|--------------------:|--------:|----------:|--------:|-------:|---------:|---------:|
| 2010 |       2891800 |      61300 |      44150 |              572000 |    5475 |     22227 |      NA |     NA |       NA |       NA |
| 2011 |       2898100 |      60880 |      47580 |              591500 |    5631 |     23781 | 2903319 |  -5219 |   592369 |     -869 |
| 2012 |       2901800 |      60130 |      47210 |              614600 |    5469 |     23997 | 2905931 |  -4131 |   615083 |     -483 |
| 2013 |       2929900 |      60730 |      47070 |              638000 |    5454 |     23508 | 2909266 |  20634 |   638302 |     -302 |
| 2014 |       2991200 |      62010 |      47870 |              659600 |    5466 |     24924 | 2938094 |  53106 |   660146 |     -546 |
| 2015 |       3063100 |      60390 |      47810 |              681300 |    5499 |     25524 | 2999841 |  63259 |   681946 |     -646 |
| 2016 |       3133600 |      59200 |      49460 |              703400 |    5640 |     25005 | 3070040 |  63560 |   704105 |     -705 |
| 2017 |       3192200 |      60330 |      49860 |              724900 |    5856 |     26880 | 3137484 |  54716 |   725980 |    -1080 |
| 2018 |       3242400 |      61100 |      51870 |              747800 |    5922 |     26694 | 3196748 |  45652 |   748066 |     -266 |
| 2019 |       3301200 |      62040 |      53690 |              776900 |    6057 |     27531 | 3245573 |  55627 |   772139 |     4761 |
| 2020 |       3330700 |      64100 |      55390 |              806200 |    5763 |     26259 | 3303787 |  26913 |   804331 |     1869 |
| 2021 |       3319500 |      67350 |      56490 |              829500 |    5952 |     28302 | 3333458 | -13958 |   833288 |    -3788 |
| 2022 |       3342300 |      68450 |      57680 |              852300 |    6396 |     31581 | 3323964 |  18336 |   854409 |    -2109 |

<a rel="delivery" href="https://github.com/BCDevExchange/docs/wiki/Project-States"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://img.shields.io/badge/BCDevExchange-Delivery-brightgreen.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>

---

# Analysis of trends in groundwater levels in BC

These scripts produce the 2013 analysis of trends in groundwater levels, presented on 
[Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/water/wells/index.html?WT.ac=GH_wells).

The analysis uses a package we developed called **bcgroundwater**, also available on [GitHub](https://github.com/bcgov/bcgroundwater).

### Usage

Data must be downloaded separately:

- Groundwater levels were downloaded from the 
  [B.C. Observation Well Network](http://www.env.gov.bc.ca/wsd/data_searches/obswell/map/obsWells.html).
- Well attribute data were downloaded from [DataBC](http://catalogue.data.gov.bc.ca/dataset/ground-water-wells-spatial-view-with-attribute-info),
  and are available under the [Open Government License–British Columbia](http://www.data.gov.bc.ca/local/dbc/docs/license/OGL-vbc2.0.pdf)
- Natural Resource Regions used in the summaries were also downloaded from 
  [DataBC](http://catalogue.data.gov.bc.ca/dataset/natural-resource-operations-regions) under the 
  [Open Government License–British Columbia](http://www.data.gov.bc.ca/local/dbc/docs/license/OGL-vbc2.0.pdf)

There are four core scripts that are required for the analysis, they need to be run in order:

- 01_clean.R
- 02_analysis.R
- 03_visualize.R
- 04_output.R

### Project Status

The results of our analysis can be downloaded as a .csv in the 
[data](http://www.env.gov.bc.ca/soe/indicators/water/wells/index.html#data-link) 
section of the indicator page.

We are not actively developing this analysis, but you can check the 
[issues](https://github.com/bcgov/groundwater_levels/issues/) for things we would 
like to fix or work on.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/groundwater_levels/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

### License

Apache 2.0. See our [license](LICENSE) for more details.

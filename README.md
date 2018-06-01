<div id="devex-badge"><a rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported."/></a>[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)</div>

# Long-term Trends in Groundwater Levels in B.C.

This repository contains [R](http://www.r-project.org) code that calculates long-term trends in groundwater levels. It supports an indicator published on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html). 

The scripts use the [bcgroundwater R package](https://github.com/bcgov/bcgroundwater/) and groundwater monitoring data from the [B.C. Observation Well Network](http://www.env.gov.bc.ca/wsd/data_searches/obswell/index.html) to:

- analyze long-term trends of groundwater levels
- produce provincial-scale and individual well summary statistics
- generate supporting data visualizations

## Usage

### Data

All the data sourced for the analysis is provided under the [Open Government Licence – British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61).

- Groundwater level monitoring data are downloaded from the 
  [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/57c55f10-cf8e-40bb-aae0-2eff311f1685) via the [`bcgroundwater` R package](https://github.com/bcgov/bcgroundwater)
- Groundwater well attribute data are downloaded directly from the
  [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/e4731a85-ffca-4112-8caf-cb0a96905778)
- Natural Resource Regions used in the summaries are sourced from the [`bcmaps` R package](https://cran.r-project.org/web/packages/bcmaps/index.html)


### Code

There are four core scripts that are required for the analysis, they need to be run in order:

- 01_clean.R
- 02_analysis.R
- 03_visualize.R
- 04_output.R


## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/groundwater-levels-indicator/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

    Copyright 2018 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC) for a complete list of our repositories on GitHub.



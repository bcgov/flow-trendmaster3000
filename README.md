[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

# Stream/River Flow (Trendmaster3000)

This repository contains a Shiny app written in R by Chris Madsen. This
app is intended to be used by anyone interested in examining the trends
in stream / river flow metrics in British Columbia over time.

To run this app, please ensure you have R and RStudio installed on your
machine. Next, navigate to the `app/` folder and open the
`flow_shiny_app.R` script. If you are using RStudio (recommended!), you
should see a small green arrow with the text `Run App` near the top of
the script window. Alternatively, you can press Control + Shift + Enter
to run the app from inside RStudio.

Once the app is running, you will be prompted to select a folder on your
computer in which to download the HYDAT database. The database will be
downloaded and some filtering files will be generated - this will only
happen the first time you run the app. Once these files have been
downloaded or generated, you are ready to explore flow trends!

#### Help and Feedback

Please contact me with any questions, issues or feedback at
Chris.Madsen@gov.bc.ca

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2023 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*

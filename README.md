# magic-modules-template
![GitHub last commit](https://img.shields.io/github/last-commit/MaGIC-Analytics/magic-dimension-reduction)
[![run with docker](https://img.shields.io/badge/run%20with-docker-0db7ed?labelColor=000000&logo=docker)](https://www.docker.com/)
[![made with Shiny](https://img.shields.io/badge/R-Shiny-blue)](https://shiny.rstudio.com/)

## Running the App
This Shiny App has been built in to a docker container for easy deployment. You can build the image yourself (and thereby customize any ports you need) after downloading it:
```
docker build -t dimreduction .
docker run -d --rm -p 8080:8080 dimreduction
#Or for testing docker run -t -i --rm -p 8080:8080 dimreduction
```
And it should be hosted at localhost:8080

## Building the App
This is a template for the various modules that will be built for applications. For each one:
- Modify the Dockerfile to include the correct library installations
- Modify the UI to include the proper library loads as well as places where it say {$TOOL} to replace with the specific tool
- Get google analytics for that tool (if desired) and replace in the GA.html file
- Build out that specific app
- Push to cloudrun following the deployment instructions. Or setup cloudrun to pull from the repo on commit/version
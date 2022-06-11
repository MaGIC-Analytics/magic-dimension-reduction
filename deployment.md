# Deployment notes
Based on rand3k repo

## CLI deployment
```
PROJECTID=$(gcloud config get-value project)
docker build . -t gcr.io/$PROJECTID/magic-$PROJECT
docker push gcr.io/$PROJECTID/magic-$PROJECT
gcloud run deploy --image gcr.io/$PROJECTID/magic-$PROJECT --platform managed --max-instances 1
```
Manually adjust CPUs and RAM applied to the container as it may be custom.


## Manual deployment
Also very easy:
- Log into your gcloud account. 
- Access cloudrun
- Hit deploy new.
- Choose the repository and set it to the dockerfile
- Define your build conditions
- Update the timeout. Default is 10 min. 
- Once built, add mapping to cloud run to add that tool
    - Copy the cname, go to cloud dns under network services. Add the record set for the new tool
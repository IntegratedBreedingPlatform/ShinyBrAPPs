# ShinyBrAPPs

## What it is

...

## Deployment via `docker` and `shinyproxy`

### Set up  a `docker` network

Create docker network for ShinyProxy to communicate with the shinyapp containers.

```
sudo docker network create shinyproxy-network
```

### Build the `shinybrapps` image

```
sudo docker build . -t shinybrapps
```


This is a docker image with `r-base` and the `shinybrapps` package and its dependencies.

### Build the shinyproxy image

```
cd ./deployment_shinyproxy/
sudo docker build . -t shinyproxy-with-config
```

### Run shinyproxy

```
sudo docker run \
-d \
-v /var/run/docker.sock:/var/run/docker.sock:ro \
--group-add $(getent group docker | cut -d: -f3) \
--net shinyproxy-network \
-p 80:8080 shinyproxy-with-config
```


See [deployment_shinyproxy/README.md](deployment_shinyproxy/README.md) for more details

## Apps

### stabrap

[http://127.0.0.1/app/stabrap/?token=TOKEN&cropDb=CROPDB&studyDbId=STUDYDBID]()

or

[http://127.0.0.1/app/stabrap/]()
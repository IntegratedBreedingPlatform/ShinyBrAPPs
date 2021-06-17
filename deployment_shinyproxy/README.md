# Deployment

## Principle

This solution is based on [shinyproxy](shinyproxy.io/), a server for R shiny apps.

`ShinyProxy` runs in an isolated container. When an app is requested via an html request, `ShinyProxy` launches a container from the `shinybrapps` docker image. This image contains `R`, the `BrAPPs` and their dependencies.

## The `shinybrapps` image

```
sudo docker build . -t shinybrapps
```

## Shinyproxy

### Config

#### Create a docker network

ShinyProxy uses a docker network to communicate with the shinyapp containers.

```
$ sudo docker network create shinyproxy-network
```

#### Set up the apps

In `deployment_shinyproxy/application.yml`, define the `shinybrapps` apps that `ShinyProxy` will connect to. 

```
  - id: test
    display-name: test app
    description: displays hello world
    container-cmd: ["R", "-e", "shinybrapps::run_test()"]
    container-image: shinybrapps
    container-network: shinyproxy-network
```

The `shinybrapps` image contains all the `shinybrapps` apps. To tell `ShinyProxy` to launch a given app, specify the container start up command `container-cmd: ["R", "-e", "shinybrapps::run_<my app>()"]`

See  [R/runapps.R](.R/runapps.R) for available apps.

#### Set up the template

ShinyProxy renders the app in an `iframe` and offers a landing page with the record of the shinyapps it can serve. The templating can be changed in `./deployment_shinyproxy/templates/`

### Build the shinyproxy docker image

Build instructions are in `./deployment_shinyproxy/Dockerfile`.

```
cd ./deployment_shinyproxy/
sudo docker build . -t shinyproxy-with-config
```


## Launch `shinyproxy`

### command

```
sudo docker run \
-d \
-v /var/run/docker.sock:/var/run/docker.sock:ro \
--group-add $(getent group docker | cut -d: -f3) \
--net shinyproxy-network \
-p 80:8080 shinyproxy-with-config
```

### detail

`-d ` *(optional)*

>by adding the -d option to the command (just after docker run), the Docker container will run in the background.
[ref](https://github.com/openanalytics/shinyproxy-config-examples/tree/master/02-containerized-docker-engine#how-to-run)

`-v /var/run/docker.sock:/var/run/docker.sock:ro `

Mount /var/run/docker.sock (a Unix domain socket) to communicate with the main Docker process. In this case, it allows shinyproxy to start and stop shiny app containers on the Docker host. [ref](https://docs.docker.com/engine/reference/commandline/run/#options)

`	--group-add $(getent group docker | cut -d: -f3) `

> inside the Docker container, ShinyProxy runs as a non-root user, therefore it does not have access to the Docker socket by default. By adding the `--group-add $(getent group docker | cut -d: -f3)` option we ensure that the user is part of the docker group and thus has access to the Docker daemon.

[ref](https://github.com/openanalytics/shinyproxy-config-examples/tree/master/02-containerized-docker-engine#how-to-run)

`getent group docker | cut -d: -f3` 

gets the docker entry in the group database.

`	--net shinyproxy-network`

Custom bridge network to allow the containers to access each other using the container ID as hostname. [ref](https://github.com/openanalytics/shinyproxy-config-examples/tree/master/02-containerized-docker-engine#how-to-run)


`	-p 80:8080 shinyproxy-with-config`

ShinyProxy listens for HTTP traffic on port 80 (or other port). [ref](https://docs.docker.com/engine/reference/commandline/run/#options)


## Update workflow

```
# create/update app image
...

# update application.yml
...

# rebuild the shinyproxy image (fast)
sudo docker build . -t shinyproxy-with-config

# relaunch the shinyproxy container
sudo docker run \
-d \
-v /var/run/docker.sock:/var/run/docker.sock:ro \
--group-add $(getent group docker | cut -d: -f3) \
--net shinyproxy-network \
-p 80:8080 shinyproxy-with-config

```

Distributed Wamp Router
=======================

Multiple Wamp Router forming a cluster, allowing clients to subscribe and
publish to any of them

based on the [simple router example](https://github.com/bwegh/erwa.git)

tl;dr
-----

distribute Wamp router across two hosts: first-machine and second-machine

``` bash
first-machine$ make

first-machine$ ./_rel/dist_wamp_router/bin/dist_wamp_router console

# now the same on second-machine
```

Then point your browser to http://first-machine:8080 and http://second-machine:8080,
connect to the default realm, subscribe to the same topic on both machines, then
publish something on that topic and watch that event appear on the other machine *tadah* 

Also have a look at [the Wamp specification](http://wamp.ws/spec) for further
informations on the possibilities.

Packaging
---------
* Run `make` requires a recent erlang. We suggest downloading it from [here](https://www.erlang-solutions.com/downloads/download-erlang-otp)
* Ensure you have `rebar` and `relx` installed and in your path.
  If not:
  ```bash
  wget https://raw.githubusercontent.com/wiki/rebar/rebar/rebar
  chmod +x rebar
  wget https://github.com/erlware/relx/releases/download/v1.0.2/relx
  chmod +x relx

  export PATH=$PATH:$(pwd)
  ```
* Build an RPM with `./build_rpm_with_fpm.sh` this requires [fpm](https://github.com/jordansissel/fpm) (`gem install fpm`).

Configuration
-------------
* `/etc/sysconfig/dist-wamp-router`
  ```bash
  USER=username-to-run-as
  ```

Run in docker
-------------
```bash
cd dist-yadtbroadcaster
git clone https://github.com/arnehilmann/baseimage
./init-devenv
export DOCKER_IMAGE=arne/riak
baseimage/start-dns
baseimage/start-node 1
baseimage/start-node 2
[...]
ssh node1.node.consul
cd /docker
_rel/dist_wamp_router/bin/dist_wamp_router attach
```


TODOS
-----

* disable ip checking when using ssh (annoying because the ip changes with every restart of a container)

* configure localhost in /etc/resolv.conf permanently (read: resolvconf does _not_ remove 'localhost' as nameserver)


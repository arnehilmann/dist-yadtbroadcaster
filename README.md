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

TODOS
-----

* disable ip checking when using ssh (annoying because the ip changes with every restart of a container)

* configure localhost in /etc/resolv.conf permanently (read: resolvconf does _not_ remove 'localhost' as nameserver)


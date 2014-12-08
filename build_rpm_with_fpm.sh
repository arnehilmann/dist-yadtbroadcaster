#!/bin/bash

# the prelinker causes problem with RPM cpio digests
# so we undo its work with `prelink -u` here
command -v prelink >/dev/null 2>&1 && prelink -u _rel/dist_wamp_router/erts-*/bin/*


MINOR_VERSION=${1:?no MINOR_VERSION specified, usage is $0 MINOR_VERSION}

cp -r _rel dist-wamp-router

fpm -t rpm -v 1.$MINOR_VERSION --directories /opt/dist-wamp-router --depends riak --depends dist-wamp-router-docroot --after-install res/postinstall/chkconfig_dist_wamp_router.sh --after-remove res/postuninstall/un-chkconfig_dist_wamp_router.sh --rpm-group admins --rpm-use-file-permissions --rpm-defattrdir 0775 -n dist-wamp-router -s dir dist-wamp-router=/opt/ res/initscript/dist-wamp-router=/etc/init.d/dist-wamp-router res/yadt/dist_wamp_router.yaml=/etc/yadt.conf.d/dist_wamp_router.yaml

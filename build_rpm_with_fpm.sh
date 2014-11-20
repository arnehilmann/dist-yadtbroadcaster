#!/bin/bash

MINOR_VERSION=${1:?no MINOR_VERSION specified}

cp -r _rel dist-wamp-router

fpm -t rpm -v 1.$MINOR_VERSION --directories /opt/dist-wamp-router --after-install res/postinstall/chkconfig_dist_wamp_router.sh --after-remove res/postuninstall/un-chkconfig_dist_wamp_router.sh --rpm-group admins --rpm-use-file-permissions --rpm-defattrdir 0775 -n dist-wamp-router -s dir dist-wamp-router=/opt/ res/initscript/dist-wamp-router=/etc/init.d/dist-wamp-router

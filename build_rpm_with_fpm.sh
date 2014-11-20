#!/bin/bash

MINOR_VERSION=${1:?no MINOR_VERSION specified}

cp -r _rel dist-wamp-router

fpm -t rpm -v 1.$MINOR_VERSION --rpm-auto-add-directories --directories /opt/dist-wamp-router --rpm-group admins --rpm-use-file-permissions  --rpm-defattrdir 0775 -n dist-wamp-router -s dir dist-wamp-router=/opt/ res/initscript/dist-wamp-router=/etc/init.d/dist-wamp-router

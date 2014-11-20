#!/bin/bash

if [ $1 = 0 ]; then
  /sbin/chkconfig --del dist-wamp-router
fi


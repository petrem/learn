#!/bin/sh

apt-get --assume-yes install rsyslog-gnutls
service rsyslog restart

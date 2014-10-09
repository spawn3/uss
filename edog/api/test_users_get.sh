#!/usr/bin/env bash

curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X GET http://192.168.1.201:9501/$1

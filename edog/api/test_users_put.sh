#!/usr/bin/env bash

curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X PUT -d "{\"id\":1}" http://192.168.1.201:9501/$1

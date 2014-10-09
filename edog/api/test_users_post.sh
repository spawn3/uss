#!/usr/bin/env bash

curl -v -H "Accept: application/json" -H "Content-Type: application/json" -X POST -d "{\"action\":\"update\", \"alias\":\"mds\", \"company\":\"mds\", \"address\":\"mds\", \"contact_person\":\"mds\", \"telephone\":\"1234567890\", \"cellphone\":\"13717670012\", \"email\":\"admin2@meidisen.com\"}" http://192.168.1.201:9501/$1

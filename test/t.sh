#!/bin/sh

curl -v -XPOST -d@$1 -H "Content-Type: application/json" \
	http://localhost:8080/g

#!/usr/bin/env bash

# Get a .png of the first page of the paper
pdftoppm -png swarm.pdf -f 1 -l 1 -r 72 www/swarm

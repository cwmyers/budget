#!/bin/bash

echo "budgetFile, masterCardCsv, qtmbCsv, ingDirectCsv, date"

sbt "run $1 $2 $3 $4 $5"

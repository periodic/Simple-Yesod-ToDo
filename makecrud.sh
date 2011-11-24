#!/bin/bash

model=$1
modelLC=`echo $1 | tr '[A-Z]' '[a-z]'`

sed -e "s/__DATA__/$model/g" -e "s/__DATALC__/$modelLC/g" Handler/CRUD.hs > Handler/$model.hs

mkdir -p hamlet/$modelLC
for file in index view create update
do
    sed -e "s/__DATA__/$model/g" -e "s/__DATALC__/$modelLC/g" hamlet/crud/$file.hamlet > hamlet/$modelLC/$file.hamlet
done

#!/bin/bash
make prologTerm >> /dev/null
function print(){
    result=$(./prologTerm "$2$1")
    printf "%s" $result
    printf "\n\n" 
    
}

for t in $(ls ../Samples/APS3/)
do
    print $t "../Samples/APS3/" 
done

make clean >> /dev/null
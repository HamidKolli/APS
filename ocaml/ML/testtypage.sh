#!/bin/bash

make prologTerm >> /dev/null

function test(){
    a="true"
    result=$(./typprog.sh "$2$1")
    
    if [ "$result" = "$a" ] ;
    then
        echo "$1 test passed" 
    else 
        echo "$1 test failed"
    fi
}

for t in $(ls ../Samples/APS0/)
do
    test $t "../Samples/APS0/" 
done

for t in $(ls ../Samples/APS1/)
do
    test $t "../Samples/APS1/" 
done

for t in $(ls ../Samples/APS2/)
do
    test $t "../Samples/APS2/" 
done

for t in $(ls ../Samples/APS3/)
do
    test $t "../Samples/APS3/" 
done

make clean >> /dev/null
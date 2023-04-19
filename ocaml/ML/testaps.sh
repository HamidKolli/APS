#!/bin/bash

make eval >> /dev/null
make prologTerm >> /dev/null
function test(){
    a="true"
    result=$(./exeprog.sh "$2$1")
    
    if [ "$result" = "$3" ] ;
    then
        echo "$1 test passed" 
    else 
        echo "$1 test failed $result"
    fi
}

for t in $(ls ../Samples/APS0/)
do
    test $t "../Samples/APS0/" 1
done

for t in $(ls ../Samples/APS1/)
do
    test $t "../Samples/APS1/" 1
done

for t in $(ls ../Samples/APS2/)
do
    test $t "../Samples/APS2/" 1
done

for t in $(ls ../Samples/APS3/)
do
    test $t "../Samples/APS3/" 1
done


make clean >> /dev/null
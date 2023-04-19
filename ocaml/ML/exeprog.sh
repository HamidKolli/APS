
a="true"
result=$(./typprog.sh $1)

if [ "$result" = "$a" ] ;
then
    ./eval $1;
else 
    echo "le programme n'est pas bien type"
fi

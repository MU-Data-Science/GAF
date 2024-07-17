#!/bin/bash

# echo "Removing old JNL file.."
# rm /mydata/dgl/general/blazegraph.jnl

blazegraph_path='/mydata/dgl/general/blazegraph.jar'
triple_properties_path='/mydata/dgl/general/triple.properties'
quad_properties_path='/mydata/dgl/general/quad.properties'

for file in "$1"/*; do
    if [[ $file == *.nq ]]; then 
        echo "Loading VCF files into Blazegraph.."
        java -Xmx4g -cp $blazegraph_path com.bigdata.rdf.store.DataLoader -verbose -namespace kb $quad_properties_path $file
    fi
done
for file in "$1"/*; do
    if [[ $file == *.ttl ]]; then 
        echo "Loading CADD_Scores files into Blazegraph.."
        java -Xmx4g -cp $blazegraph_path com.bigdata.rdf.store.DataLoader -verbose -defaultGraph http://sg.org -namespace kb $triple_properties_path $file
    fi
done

echo "Starting up Blazegraph.."
java -server -Xmx4g -jar $blazegraph_path
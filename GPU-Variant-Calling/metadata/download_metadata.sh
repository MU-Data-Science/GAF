# File to save the metadata

metadata_file="metadata.csv"

# Loop over each sample in the array
for id in $(cat sample_ids.txt)
do
    echo "Fetching metadata for $id"
    # Fetch the metadata for the sample
    if esearch -db sra -query "$id" | efetch -format runinfo >> "$metadata_file"
    then
        echo "Successfully fetched metadata for $id"
    else
        echo "Failed to fetch metadata for $id"
        exit 1
    fi
    # Add a delay
    sleep 0.5
done

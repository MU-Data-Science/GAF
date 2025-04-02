
# Download latest version
wget https://snpeff.blob.core.windows.net/versions/snpEff_latest_core.zip

# Unzip file
unzip snpEff_latest_core.zip
rm snpEff_latest_core.zip

#download dataset
cd ./snpEff/
wget https://snpeff.blob.core.windows.net/databases/v5_1/snpEff_v5_1_GRCh38.p14.zip
unzip snpEff_v5_1_GRCh38.p14.zip
cd ..

#moving snpEff
mv snpEff ./tools/







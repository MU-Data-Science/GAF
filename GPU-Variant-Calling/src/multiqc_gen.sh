rm -r -f ./results/multiqc_report_*.html
rm -r -f ./results/multiqc_data*
{
mkdir -p ./fastqc_reports
for i in ./data/*;
	do fastqc $i -o ./fastqc_reports;done
multiqc ./fastqc_reports/ -o ./results
mv ./results/multiqc_report.html ./results/multiqc_data/
echo "Multiqc files have been generated and stored in results folder 😄"
} 2>&1 | tee ./logs/multiqc_gen.logs



{
python3 feature_extract.py
} 2>&1 | tee ./logs/feature_extract.logs

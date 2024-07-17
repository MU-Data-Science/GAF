'''
Developed by Shivika Prasanna on 01/25/2022.
Last updated on 01/25/2022.
Reads VCF files and annotates using SNPEFF jar file. Stores output in the given argument.
Working code. 
Run in terminal as: python3 Code/ProcessFiles.py -i <input folder> -j <jar file>
input_folder = /path/to/Variant_Analysis_Output_Mar_8_2021_hg19/unzipped-VCF
jar_file = /path/to/snpEff/snpeff.jar

EX: python3 ProcessFiles.py -i /Users/shivikaprasanna/Desktop/Mizzou_Academics/GRA.nosync/Spring22-GRA/NSF-Rapid-Genomics/Variant_Analysis_Output_Mar_8_2021_hg19/sample/unzipped -o /Users/shivikaprasanna/Desktop/Mizzou_Academics/GRA.nosync/Spring22-GRA/NSF-Rapid-Genomics/Variant_Analysis_Output_Mar_8_2021_hg19/sample/annotated -j /Users/shivikaprasanna/Desktop/Mizzou_Academics/GRA.nosync/Spring22-GRA/NSF-Rapid-Genomics/snpEff/snpeff.jar
'''
import os, subprocess, shlex
from pathlib import Path

def annotate_vcf(input_vcf, jar_file):
    output_vcf_file = input_vcf.replace('.vcf', '_ann_hg19.vcf')
    with open (output_vcf_file, 'w+') as f:
        subprocess.call(shlex.split('java -Xmx8g -jar ' + jar_file + ' -v hg19 ' + input_vcf), stdout=f)
                
    print("Completed processing VCF file and writing it as an N3 file!")
    return output_vcf_file
    
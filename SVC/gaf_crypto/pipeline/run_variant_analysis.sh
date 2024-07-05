#!/usr/bin/env bash

if [[ $# -ne 3 ]]; then
    echo "Usage: run_variant_analysis.sh <filename> <caller_type> <flag>"
    echo "Required arguments:"
    echo "  filename: interleaved FASTQ file" 
    echo "  caller_type: "F" for Freebayes; "H" for HaplotypeCaller"
    echo "  flag: 0 if input/output files are plaintext; 1 if input/output files are ciphertext"
    exit 1
fi

SEQID="output"
REF="hs38.fa"
DATADIR=${HOME}
let NUM_BWA_THREADS=$(nproc)-3
CIPHER_FLAG=${3}

BWA=${HOME}/bwa/bwa
PICARDJAR=${HOME}/picard/picard.jar
BAM_INDEX=${HOME}/${SEQID}.bai
GATK=${HOME}/gatk-4.2.6.0/gatk
SAMTOOLS=${HOME}/samtools-1.18/samtools
FREEBAYES=${HOME}/freebayes

echo "👉 Starting variant calling @ "$(date)
echo "CIPHER_FLAG: "${CIPHER_FLAG}

if [[ $CIPHER_FLAG == "1" ]]; then
    # Decrypt the FASTQ file and invoke BWA
    FASTQ_EN=${1}
    PIPE_1="/tmp/f3"
    PIPE_2="/tmp/f1"
    CRYPTO_PIPE="/tmp/gaf_crypto"
    SAM_EN=${DATADIR}/${SEQID}.sam.gz.en
    SAM_RG_EN=${DATADIR}/${SEQID}-rg.sam.gz.en
    SAM_SORT_EN=${DATADIR}/${SEQID}-rg-sorted.bam.en
    SAM_FINAL_EN=${DATADIR}/${SEQID}-rg-sorted-final.bam.en
    VCF_EN=${DATADIR}/${SEQID}.vcf.en

    # Create the required named pipes
    rm -rf ${PIPE_1}
    rm -rf ${PIPE_2}
    mkfifo ${PIPE_1}; chmod 0777 ${PIPE_1}
    mkfifo ${PIPE_2}; chmod 0777 ${PIPE_2}

    # BWA
    echo "${FASTQ_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0
    echo "${PIPE_2} ${SAM_EN} ENCRYPT" > ${CRYPTO_PIPE}_1

    ${BWA} mem -t ${NUM_BWA_THREADS} ${REF} ${PIPE_1} | gzip > ${PIPE_2}
    #comment
    echo "👉 Done with BWA [ciphertext] "$(date)

    # Invoke picard.jar
    # Read groups
    echo "${SAM_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0
    echo "${PIPE_2} ${SAM_RG_EN} ENCRYPT" > ${CRYPTO_PIPE}_1

    java -jar ${PICARDJAR} AddOrReplaceReadGroups \
        I=${PIPE_1} \
        O=${PIPE_2} \
        RGSM=mysample \
        RGPU=myunit \
        RGID=mygroupID \
        RGLB=mylib \
        RGPL=Illumina

    rm -rf ${SAM_EN}
    echo "👉 Done with ReadGroups [ciphertext] "$(date)

    # SortSam
    echo "${SAM_RG_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0
    echo "${PIPE_2} ${SAM_SORT_EN} ENCRYPT" > ${CRYPTO_PIPE}_1

    java -jar ${PICARDJAR} SortSam \
        I=${PIPE_1} \
        O=${PIPE_2} \
        SORT_ORDER=coordinate

    rm -rf ${SAM_RG_EN}
    echo "👉 Done with SortSam [ciphertext] "$(date)

    # Mark duplicates
    echo "${SAM_SORT_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0
    echo "${PIPE_2} ${SAM_FINAL_EN} ENCRYPT" > ${CRYPTO_PIPE}_1

    ${SAMTOOLS} markdup ${PIPE_1} ${PIPE_2}

    # java -jar ${PICARDJAR} MarkDuplicates \
    #     -I ${PIPE_1} \
    #     -O ${PIPE_2} \
    #     -M /tmp/dup_metric.txt

    rm -rf ${SAM_SORT_EN}
    echo "👉 Done with MarkDuplicates [ciphertext] "$(date)
    #comment

    # Need bamindex for HaplotypeCaller
    if [[ ${2} == "H" ]]; then
        # bamindex
        echo "${SAM_FINAL_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0

        java -jar ${PICARDJAR} BuildBamIndex \
            -I ${PIPE_1} \
            -O ${BAM_INDEX}

        echo "👉 Done with BamIndex [ciphertext] "$(date)
    fi

    # Variant caller
    echo "${SAM_FINAL_EN} ${PIPE_1} DECRYPT" > ${CRYPTO_PIPE}_0
    echo "${PIPE_2} ${VCF_EN} ENCRYPT" > ${CRYPTO_PIPE}_1

    if [[ ${2} == "F" ]]; then
        # Freebayes
        ${FREEBAYES} -f ${REF} ${PIPE_1} > ${PIPE_2}
        echo "👉 Done with Freebayes [ciphertext] "$(date)
    elif [[ ${2} == "H" ]]; then
        # HaplotypeCaller
        ${GATK} HaplotypeCaller \
            -I ${PIPE_1} \
            -O ${PIPE_2} \
            -R ${REF} \
            --read-index ${BAM_INDEX}
        echo "👉 Done with HaplotypeCaller [ciphertext] "$(date)
    else
        echo "Unknown caller: "${2}
        exit 1
    fi
elif [[ $CIPHER_FLAG == "0" ]]; then
    FASTQ_PLAIN=${1}
    SAM_PLAIN=${DATADIR}/${SEQID}.sam.gz
    SAM_RG_PLAIN=${DATADIR}/${SEQID}-rg.sam.gz
    SAM_SORT_PLAIN=${DATADIR}/${SEQID}-rg-sorted.bam
    SAM_FINAL_PLAIN=${DATADIR}/${SEQID}-rg-sorted-final.bam
    VCF_PLAIN=${DATADIR}/${SEQID}.vcf

    ${BWA} mem -t ${NUM_BWA_THREADS} ${REF} ${FASTQ_PLAIN} | gzip > ${SAM_PLAIN}
    
    echo "👉 Done with BWA [plaintext] "$(date)

    # Invoke picard.jar
    # Read groups
    java -jar ${PICARDJAR} AddOrReplaceReadGroups \
        I=${SAM_PLAIN} \
        O=${SAM_RG_PLAIN} \
        RGSM=mysample \
        RGPU=myunit \
        RGID=mygroupID \
        RGLB=mylib \
        RGPL=Illumina

    rm -rf ${SAM_PLAIN}
    echo "👉 Done with ReadGroups [plaintext] "$(date)

    # SortSam
    java -jar ${PICARDJAR} SortSam \
        I=${SAM_RG_PLAIN} \
        O=${SAM_SORT_PLAIN} \
        SORT_ORDER=coordinate

    rm -rf ${SAM_RG_PLAIN}
    echo "👉 Done with SortSam [plaintext] "$(date)

    # Mark duplicates
    ${SAMTOOLS} markdup ${SAM_SORT_PLAIN} ${SAM_FINAL_PLAIN}

    rm -rf ${SAM_SORT_PLAIN}
    echo "👉 Done with MarkDuplicates [plaintext] "$(date)

    # Need bamindex for HaplotypeCaller
    if [[ ${2} == "H" ]]; then
        # bamindex
        java -jar ${PICARDJAR} BuildBamIndex \
            -I ${SAM_FINAL_PLAIN} \
            -O ${BAM_INDEX}

        echo "👉 Done with BamIndex [plaintext] "$(date)
    fi

    # Variant caller
    if [[ ${2} == "F" ]]; then
        # Freebayes
        ${FREEBAYES} -f ${REF} ${SAM_FINAL_PLAIN} > ${VCF_PLAIN}
        echo "👉 Done with Freebayes [plaintext] "$(date)
    elif [[ ${2} == "H" ]]; then
        # HaplotypeCaller
        ${GATK} HaplotypeCaller \
            -I ${SAM_FINAL_PLAIN} \
            -O ${VCF_PLAIN} \
            -R ${REF} \
            --read-index ${BAM_INDEX}
        echo "👉 Done with HaplotypeCaller [plaintext] "$(date)
    else
        echo "Unknown caller: "${2}
        exit 1
    fi
else
    echo "Unsupported flag: "${CIPHER_FLAG}
fi

echo "👉 Done with variant calling @ "$(date)
/**
 * University of Missouri-Columbia
 * 2020
 */

import java.util.Calendar
import sys.process._

object GenomeTasks {

  // Download
  def runDownload[T](x: T):T = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting to download $x at $beginTime")
    val outputFileName = x.toString.split("/").last
    println("Output filename: ", outputFileName)
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val ret = (Seq("curl", "-sS", s"$x") #| Seq(s"$hdfsCmd", "dfs", "-put", "-", s"/$outputFileName")).!
    val curlCmd =
      s"curl -sS $x | " + sys.env("HADOOP_HOME") + s"/bin/hdfs dfs -put - /$outputFileName "
    val endTime = Calendar.getInstance().getTime()
    println(s"Completed download command: $curlCmd $ret at $endTime")
    x
  }

  // GATK pipeline stages

  // Simple function to parse sample information
  def splitSampleInfo(s: String):(String, Int) = {
    val temp = s.split("-")
    (temp(0), temp(1).toInt)
  }

  // Construct .bam file from FASTQ
  def runFastqToBam[T](x: T, referenceGenome: String, useGPUs: Boolean, numBuckets: Int, useFCFS: Boolean, useSomatic: Boolean):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()

    println(s"Starting BAM_construction on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)
    println(s"BAM_partitionID: $sampleID $partitionID")

    var retBam = -1
    val hdfsPrefix = "hdfs://vm0:9000"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val gatk = sys.env("GATK_HOME") + "/gatk"
    val dataDir = sys.env("DATA_DIR")

    val GPU_alloc_mode = if (useFCFS == true) 0 else 1 // 0: first-come, first-served; 1: execute an entire RDD partition on a GPU

    val normal = "normal"
    val tumor = "tumor"

    try {
      var retBam0 = -1
      var retBam1 = -1

      // Somatic variant calling
      if (useSomatic == true) {
        // Remove if already present
        retBam0 = Seq("rm", "-f", s"$dataDir/$sampleID" + s"_$normal" + "_1.fastq.gz", s"$dataDir/$sampleID" + s"_$normal" + "_2.fastq.gz").!
        println(s"DeletingFASTQ $retBam0 $sampleID $normal")
        // Copy the FASTQ files to local storage
        retBam1 = Seq(s"$hdfsCmd", "dfs", "-get", s"$hdfsPrefix/${sampleID}" + s"_$normal" + "_?.fastq.gz", s"$dataDir").!
        println(s"CopyingFASTQ $retBam1 $sampleID $normal")

        retBam0 += Seq("rm", "-f", s"$dataDir/$sampleID" + s"_$tumor" + "_1.fastq.gz", s"$dataDir/$sampleID" + s"_$tumor" + "_2.fastq.gz").!
        println(s"DeletingFASTQ $retBam0 $sampleID $tumor")
        // Copy the FASTQ files to local storage
        retBam1 += Seq(s"$hdfsCmd", "dfs", "-get", s"$hdfsPrefix/${sampleID}" + s"_$tumor" + "_?.fastq.gz", s"$dataDir").!
        println(s"CopyingFASTQ $retBam1 $sampleID $tumor")
      }
      else { // Germline variant calling
        // Remove if already present
        retBam0 = Seq("rm", "-f", s"$dataDir/$sampleID" + "_1.fastq.gz", s"$dataDir/$sampleID" + "_2.fastq.gz").!
        println(s"DeletingFASTQ $retBam0 $sampleID")
        // Copy the FASTQ files to local storage
        retBam1 = Seq(s"$hdfsCmd", "dfs", "-get", s"$hdfsPrefix/${sampleID}" + "_?.fastq.gz", s"$dataDir").!
        println(s"CopyingFASTQ $retBam1 $sampleID")
      }

      // Convert to .bam
      if (useGPUs == true && partitionID % numBuckets == 0) {
        println(s"Starting Parabricks_fq2sam $sampleID $partitionID")
        val lockTimeout =
          GPU_alloc_mode match {
            case 0 => -1
            case 1 => 30
            case _ => 0
          }

        var retBam2 = -1

        if (useSomatic == true) {
          retBam2 = Seq(s"$dataDir/run_parabricks.sh", "fq2bam", s"$lockTimeout", s"$dataDir:/workdir",
            s"$dataDir:/outputdir", s"${referenceGenome}.fa", s"${sampleID}" + s"_$normal" + "_1.fastq.gz",
            s"${sampleID}" + s"_$normal" + "_2.fastq.gz", s"${sampleID}" + s"_$normal" + "-final-sorted-bqsr.bam").!

          retBam2 += Seq(s"$dataDir/run_parabricks.sh", "fq2bam", s"$lockTimeout", s"$dataDir:/workdir",
            s"$dataDir:/outputdir", s"${referenceGenome}.fa", s"${sampleID}" + s"_$tumor" + "_1.fastq.gz",
            s"${sampleID}" + s"_$tumor" + "_2.fastq.gz", s"${sampleID}" + s"_$tumor" + "-final-sorted-bqsr.bam").!
        }
        else {
          retBam2 = Seq(s"$dataDir/run_parabricks.sh", "fq2bam", s"$lockTimeout", s"$dataDir:/workdir",
            s"$dataDir:/outputdir", s"${referenceGenome}.fa", s"${sampleID}" + "_1.fastq.gz",
            s"${sampleID}" + "_2.fastq.gz", s"${sampleID}" + "-final-sorted-bqsr.bam").!
        }

        // If successful
        if (retBam2 == 0) {
          var retBam3 = -1

          // Copy .bam* to HDFS; include .bam.bai
          if (useSomatic == true) {
            // Copy .bam* to HDFS; include .bam.bai
            retBam3 = Seq(s"$hdfsCmd", "dfs", "-put",
              s"$dataDir/${sampleID}_${normal}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}_${normal}-final-sorted-bqsr.bam.bai",
              s"$hdfsPrefix/").!
            retBam3 += Seq(s"$hdfsCmd", "dfs", "-put",
              s"$dataDir/${sampleID}_${tumor}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}_${tumor}-final-sorted-bqsr.bam.bai",
              s"$hdfsPrefix/").!
          }
          else {
            retBam3 = Seq(s"$hdfsCmd", "dfs", "-put",
              s"$dataDir/${sampleID}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}-final-sorted-bqsr.bam.bai",
              s"$hdfsPrefix/").!
          }
          println(s"Completed Parabricks_fq2bam and copying BAM: $retBam2 $retBam3 $sampleID")
          println(s"Completed-Parabricks_fq2bam-$sampleID")

          var retDel = -1
          // Delete from $dataDir
          if (useSomatic == true) {
            retDel = Seq("sudo", "rm", "-f",
              s"$dataDir/${sampleID}_${normal}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}_${normal}-final-sorted-bqsr.bam.bai").!
            retDel += Seq("sudo", "rm", "-f",
              s"$dataDir/${sampleID}_${tumor}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}_${tumor}-final-sorted-bqsr.bam.bai").!
          }
          else {
            retDel = Seq("sudo", "rm", "-f",
              s"$dataDir/${sampleID}-final-sorted-bqsr.bam",
              s"$dataDir/${sampleID}-final-sorted-bqsr.bam.bai").!
          }
          retBam = retBam2 + retBam3
        }
        else {
          println(s"Unable to execute Parabricks_fq2sam for $sampleID: $retBam2")
          retBam = retBam2
        }
      }

      // Switch to CPU if no GPUs or GPU execution failed
      if (retBam != 0) {
        println(s"Starting GATK_FastqToSam $sampleID")
        var retMkdir = -1
        var retBam2 = -1

        if (useSomatic == true) {
          retMkdir = Seq("mkdir", s"$dataDir/tmp_${sampleID}_${normal}").!
          retBam2 = Seq(s"$gatk", "FastqToSam",
            "-F1", s"$dataDir/$sampleID" + s"_${normal}" + "_1.fastq.gz",
            "-F2", s"$dataDir/$sampleID" + s"_${normal}" + "_2.fastq.gz",
            "-O", s"$dataDir/${sampleID}_${normal}-unaligned.bam",
            "--SAMPLE_NAME", "normalsample",
            "--READ_GROUP_NAME", "mygroup",
            "--PLATFORM", "illumina",
            "--LIBRARY_NAME", "mylib",
            "--TMP_DIR", s"$dataDir/tmp_${sampleID}_${normal}").!
          println(s"FastqToSamCreation $retBam2 $sampleID $normal")

          retMkdir += Seq("mkdir", s"$dataDir/tmp_${sampleID}_${tumor}").!
          retBam2 += Seq(s"$gatk", "FastqToSam",
            "-F1", s"$dataDir/$sampleID" + s"_${tumor}" + "_1.fastq.gz",
            "-F2", s"$dataDir/$sampleID" + s"_${tumor}" + "_2.fastq.gz",
            "-O", s"$dataDir/${sampleID}_${tumor}-unaligned.bam",
            "--SAMPLE_NAME", "tumorsample",
            "--READ_GROUP_NAME", "mygroup",
            "--PLATFORM", "illumina",
            "--LIBRARY_NAME", "mylib",
            "--TMP_DIR", s"$dataDir/tmp_${sampleID}_${tumor}").!
          println(s"FastqToSamCreation $retBam2 $sampleID $tumor")
        }
        else {
          retMkdir = Seq("mkdir", s"$dataDir/tmp_$sampleID").!
          retBam2 = Seq(s"$gatk", "FastqToSam",
            "-F1", s"$dataDir/$sampleID" + "_1.fastq.gz",
            "-F2", s"$dataDir/$sampleID" + "_2.fastq.gz",
            "-O", s"$dataDir/${sampleID}-unaligned.bam",
            "--SAMPLE_NAME", "mysample",
            "--READ_GROUP_NAME", "mygroup",
            "--PLATFORM", "illumina",
            "--LIBRARY_NAME", "mylib",
            "--TMP_DIR", s"$dataDir/tmp_$sampleID").!
          println(s"FastqToSamCreation $retBam2 $sampleID")
        }

        // Copy .bam to HDFS
        var retBam3 = -1

        if (useSomatic == true) {
          retBam3 = Seq(s"$hdfsCmd", "dfs", "-put", s"$dataDir/${sampleID}_${normal}-unaligned.bam", s"$hdfsPrefix/").!
          println(s"CopyingBAM $retBam3 $sampleID $normal")

          retBam3 += Seq(s"$hdfsCmd", "dfs", "-put", s"$dataDir/${sampleID}_${tumor}-unaligned.bam", s"$hdfsPrefix/").!
          println(s"CopyingBAM $retBam3 $sampleID $tumor")
        }
        else {
          retBam3 = Seq(s"$hdfsCmd", "dfs", "-put", s"$dataDir/${sampleID}-unaligned.bam", s"$hdfsPrefix/").!
          println(s"CopyingBAM $retBam3 $sampleID")
        }

        // Delete from $dataDir
        var retDel = -1

        if (useSomatic == true) {
          retDel = Seq("rm", "-f", s"$dataDir/${sampleID}_${normal}-unaligned.bam", s"$dataDir/tmp_${sampleID}_${normal}").!
          retDel += Seq("rm", "-f", s"$dataDir/${sampleID}_${tumor}-unaligned.bam", s"$dataDir/tmp_${sampleID}_${tumor}").!
        }
        else {
          retDel = Seq("rm", "-f", s"$dataDir/${sampleID}-unaligned.bam", s"$dataDir/tmp_$sampleID").!
        }

        retBam = retBam2 + retBam3
        println(s"Completed GATK_FastqToSam $sampleID: $retBam")
        println(s"Completed-GATK_FastqToSam-$sampleID")
      }
    } catch {
      case e: Exception => print(s"Exception in FastqToBam, check sequence ID $x")
    }

    // Delete from $dataDir
    var retDel = -1

    if (useSomatic == true) {
      retDel = Seq("rm", "-f", s"$dataDir/${sampleID}_${normal}_1.fastq.gz", s"$dataDir/${sampleID}_${normal}_2.fastq.gz").!
      retDel += Seq("rm", "-f", s"$dataDir/${sampleID}_${tumor}_1.fastq.gz", s"$dataDir/${sampleID}_${tumor}_2.fastq.gz").!
    }
    else {
      retDel = Seq("rm", "-f", s"$dataDir/${sampleID}_1.fastq.gz", s"$dataDir/${sampleID}_2.fastq.gz").!
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed BAM construction on ($x) at ${endTime}, return values: $retBam")
    println(s"Completed-BAM-$x")
    (x, retBam)
  }

  // BWA with MarkDuplicates
  def runBWAMarkDuplicates[T](x: T, referenceGenome: String, useGPUs: Boolean, useSomatic: Boolean):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting BWAMarkDuplicates on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val hdfsPrefix = "hdfs://vm0:9000"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val gatk = sys.env("GATK_HOME") + "/gatk"
    val dataDir = "file://" + sys.env("DATA_DIR")
    val dataDirLocal = sys.env("DATA_DIR") // Needed for local file operations via shell

    val normal = "normal"
    val tumor = "tumor"

    if (useGPUs == true) {
      println("Do nothing!")
    }

    var retBWA = -1
    try {
      // Delete $sampleId.bam_* files
      //val retDel = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.bam_*").!

      // Test if input file exists before launching the job
      var retFileExists = -1

      if (useSomatic == true) {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${normal}-unaligned.bam").!

        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
          val execBWA = Seq(s"$gatk", "BwaAndMarkDuplicatesPipelineSpark",
            "-I", s"$hdfsPrefix/${sampleID}_${normal}-unaligned.bam",
            "-O", s"$hdfsPrefix/$sampleID" + s"_${normal}"+ "-final.bam",
            "-R", s"$dataDir/$referenceGenome.fa",
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${normal}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "8",
            "--conf", "spark.executor.memory=24g",
            "--conf", "spark.executor.memoryOverhead=5g").!
          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
          retBWA = execBWA
        }
        else {
          println(s"Skipping BwaAndMarkDuplicatesPipelineSpark for ${sampleID}_${normal}")
        }

        retFileExists += Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${tumor}-unaligned.bam").!

        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
          val execBWA = Seq(s"$gatk", "BwaAndMarkDuplicatesPipelineSpark",
            "-I", s"$hdfsPrefix/${sampleID}_${tumor}-unaligned.bam",
            "-O", s"$hdfsPrefix/$sampleID" + s"_${tumor}" + "-final.bam",
            "-R", s"$dataDir/$referenceGenome.fa",
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${tumor}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "8",
            "--conf", "spark.executor.memory=24g",
            "--conf", "spark.executor.memoryOverhead=5g").!
          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
          retBWA += execBWA
        }
        else {
          println(s"Skipping BwaAndMarkDuplicatesPipelineSpark for ${sampleID}_${tumor}")
        }
      }
      else {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}-unaligned.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_$sampleID").!
          val execBWA = Seq(s"$gatk", "BwaAndMarkDuplicatesPipelineSpark",
            "-I", s"$hdfsPrefix/${sampleID}-unaligned.bam",
            "-O", s"$hdfsPrefix/$sampleID" + "-final.bam",
            "-R", s"$dataDir/$referenceGenome.fa",
            "--tmp-dir", s"$dataDir/tmp_$sampleID",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "8",
            "--conf", "spark.executor.memory=24g",
            "--conf", "spark.executor.memoryOverhead=5g").!
          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_$sampleID").!
          retBWA = execBWA
        }
      }
    }
    catch {
      case e: Exception => print(s"Exception in BWA w/ Mark Duplicates, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed BWAMarkDuplicates on ($x) ended at $endTime; return values bwa: $retBWA")
    println(s"Completed-BWAMarkDuplicates-$x")

    (x, retBWA)
  }

  // SortSam before invoking HaplotypeCaller
  def runSortSam[T](x: T, useGPUs: Boolean, useSomatic: Boolean):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting SortSam on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val hdfsPrefix = "hdfs://vm0:9000"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val gatk = sys.env("GATK_HOME") + "/gatk"
    val dataDir = "file://" + sys.env("DATA_DIR")
    val dataDirLocal = sys.env("DATA_DIR") // Needed for local file operations via shell

    val normal = "normal"
    val tumor = "tumor"

    if (useGPUs == true) {
      println("Do nothing!")
    }

    var retSortSam = -1
    try {
      // Test if input file exists before launching the job
      var retFileExists = -1

      if (useSomatic == true) {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${normal}-final.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
          retSortSam = Seq(s"$gatk", "SortSamSpark",
            "-I", s"$hdfsPrefix/${sampleID}_${normal}-final.bam",
            "-O", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted.bam", // change to final-sorted-bqsr.bam if skipping BQSR step
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${normal}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--conf", "spark.executor.memory=12g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
        }
        else {
          println(s"Skipping SortSam for ${sampleID}_${normal}")
        }

        retFileExists += Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${tumor}-final.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
          retSortSam += Seq(s"$gatk", "SortSamSpark",
            "-I", s"$hdfsPrefix/${sampleID}_${tumor}-final.bam",
            "-O", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted.bam", // change to final-sorted-bqsr.bam if skipping BQSR step
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${tumor}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--conf", "spark.executor.memory=12g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
        }
        else {
          println(s"Skipping SortSam for ${sampleID}_${tumor}")
        }
      }
      else {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}-final.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_$sampleID").!
          retSortSam = Seq(s"$gatk", "SortSamSpark",
            "-I", s"$hdfsPrefix/${sampleID}-final.bam",
            "-O", s"$hdfsPrefix/${sampleID}-final-sorted.bam", // change to final-sorted-bqsr.bam if skipping BQSR step
            "--tmp-dir", s"$dataDir/tmp_$sampleID",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--conf", "spark.executor.memory=12g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_$sampleID").!
        }
      }
    }
    catch {
      case e: Exception => print(s"Exception in SortSam, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed SortSam on ($x) ended at $endTime; return values SortSam: $retSortSam")
    println(s"Completed-SortSam-$x")
    (x, retSortSam)
  }

  // BQSR before invoking VariantCaller
  def runBQSR[T](x: T, referenceGenome: String, useGPUs: Boolean, useSomatic: Boolean): (T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting GATK_BQSR on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val hdfsPrefix = "hdfs://vm0:9000"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val gatk = sys.env("GATK_HOME") + "/gatk"
    val dataDir = "file://" + sys.env("DATA_DIR")
    val dataDirLocal = sys.env("DATA_DIR") // Needed for local file operations via shell
    val knownIndels = "file://" + sys.env("KNOWN_INDELS")
    val knownSNPs = "file://" + sys.env("KNOWN_SNPS")

    val normal = "normal"
    val tumor = "tumor"

    //    if (useGPUs == "true") {
    //      println(s"Skipped SortSam on ($x)")
    //      return (x, 0)
    //    }

    var retBQSR = -1
    try {
      // Test if input file exists before launching the job
      var retFileExists = -1

      if (useSomatic == true) {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
          retBQSR = Seq(s"$gatk", "BQSRPipelineSpark",
            "-R", s"$dataDir/$referenceGenome.fa",
            "-I", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted.bam",
            "-O", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted-bqsr.bam",
            "--known-sites", s"${knownIndels}",
            "--known-sites", s"${knownSNPs}",
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${normal}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "6",
            "--conf", "spark.executor.memory=20g",
            "--conf", "spark.executor.memoryOverhead=5g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${normal}").!
        }
        else {
          println(s"Skipping BQSR for ${sampleID}_${normal}")
        }

        retFileExists += Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
          retBQSR += Seq(s"$gatk", "BQSRPipelineSpark",
            "-R", s"$dataDir/$referenceGenome.fa",
            "-I", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted.bam",
            "-O", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted-bqsr.bam",
            "--known-sites", s"${knownIndels}",
            "--known-sites", s"${knownSNPs}",
            "--tmp-dir", s"$dataDir/tmp_${sampleID}_${tumor}",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "6",
            "--conf", "spark.executor.memory=20g",
            "--conf", "spark.executor.memoryOverhead=5g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}_${tumor}").!
        }
        else {
          println(s"Skipping BQSR for ${sampleID}_${tumor}")
        }
      }
      else {
        retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}-final-sorted.bam").!
        if (retFileExists == 0) {
          val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_$sampleID").!
          retBQSR = Seq(s"$gatk", "BQSRPipelineSpark",
            "-R", s"$dataDir/$referenceGenome.fa",
            "-I", s"$hdfsPrefix/${sampleID}-final-sorted.bam",
            "-O", s"$hdfsPrefix/${sampleID}-final-sorted-bqsr.bam",
            "--known-sites", s"${knownIndels}",
            "--known-sites", s"${knownSNPs}",
            "--tmp-dir", s"$dataDir/tmp_$sampleID",
            "--", "--spark-runner", "SPARK",
            "--spark-master", "yarn",
            "--num-executors", "6",
            "--conf", "spark.executor.memory=20g",
            "--conf", "spark.executor.memoryOverhead=5g").!

          // Delete tmp directory
          val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_$sampleID").!
        }
      }
    }
    catch {
      case e: Exception => print(s"Exception in BQSR, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed GATK_BQSR on ($x) ended at $endTime; return values BQSR: $retBQSR")
    println(s"Completed-GATK_BQSR-$x")

    (x, retBQSR)
  }

  // HaplotypeCaller (germline) or Mutect2 (somatic)
  def runVariantCaller[T](x: T, referenceGenome: String, useGPUs: Boolean, useSomatic: Boolean):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    if (useSomatic == true) {
      println(s"Starting GATK_Mutect2 on ($x) at $beginTime")
    }
    else {
      println(s"Starting GATK_HaplotypeCaller on ($x) at $beginTime")
    }
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val hdfsPrefix = "hdfs://vm0:9000"
    val dataDir = "file://" + sys.env("DATA_DIR")
    val dataDirLocal = sys.env("DATA_DIR") // Needed for local file operations via shell
    val gatk = sys.env("GATK_HOME") + "/gatk"

    val normal = "normal"
    val tumor = "tumor"

    var retVariantCaller = -1
    if (useGPUs == true) {
      println("Do nothing!")
    }

    try {

      //if (useGPUs == "true") {
      if (false) {
        // Need to copy the .bam file from HDFS to local
        println(s"Starting Parabricks_Haplotypecaller")
        val lockTimeout = 10000
        retVariantCaller = Seq(s"$dataDir/run_parabricks.sh", "haplotypecaller", s"$lockTimeout", s"$dataDir:/workdir",
          s"$dataDir:/outputdir", s"${referenceGenome}.fa", s"${sampleID}-final-sorted-bqsr.bam",
          s"${sampleID}.vcf").!

        if (retVariantCaller == 0) {
          val retCopyvcf = Seq(s"$hdfsCmd", "dfs", "-put", s"${dataDir}/${sampleID}.vcf", s"/${sampleID}.vcf").!
        }
        println(s"Done with Parabricks_Haplotypecaller")
      }

      if (retVariantCaller != 0) {
        // Test if input file exists before launching the job
        var retFileExists = -1

        if (useSomatic == true) {
          retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted-bqsr.bam").!
          print(s"RetFileExists1: $retFileExists")
          retFileExists += Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted-bqsr.bam").!
          print(s"RetFileExists2: $retFileExists")

          if (retFileExists == 0) {
            // Copy the files to $dataDir
            var copyToLocal = Seq(s"$hdfsCmd", "dfs", "-get", s"$hdfsPrefix/${sampleID}_${normal}-final-sorted-bqsr.bam*", s"${dataDir}").!
            copyToLocal = Seq(s"$hdfsCmd", "dfs", "-get", s"$hdfsPrefix/${sampleID}_${tumor}-final-sorted-bqsr.bam*", s"${dataDir}").!
            val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_${sampleID}").!
            println("Invoking Mutect2/GATK")
            retVariantCaller = Seq(s"$gatk",
              "--java-options", "-Xmx20g",
              "Mutect2",
              "-R", s"$dataDir/$referenceGenome.fa",
              "-I", s"$dataDir/${sampleID}_${normal}-final-sorted-bqsr.bam",
              "-I", s"$dataDir/${sampleID}_${tumor}-final-sorted-bqsr.bam",
              "-normal", "normalsample",
              "-O", s"$dataDirLocal/${sampleID}.vcf.gz", // Does not like $dataDir
              "-L", "chr1",
              "--native-pair-hmm-threads", "8",
              "--tmp-dir", s"$dataDir/tmp_${sampleID}").!
            println(s"Mutect2/GATK caller: $retVariantCaller")

            // for hg38, access them using gs://gatk-best-practices/somatic-hg38/1000g_pon.hg38.vcf.gz

            // Copy .vcf file to HDFS
            val copyToHDFS = Seq(s"$hdfsCmd", "dfs", "-put", s"$dataDir/${sampleID}.vcf.gz", s"$hdfsPrefix/").!

            // Delete the .vcf.gz file in local dir
            val retDelVCF = Seq("rm", "-rf", s"$dataDirLocal/${sampleID}.vcf.gz*").!

            // Delete tmp directory
            val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_${sampleID}").!
          }
          else {
            println(s"Skipping Mutect2 for ${sampleID}")
          }
        }
        else {
          retFileExists = Seq(s"$hdfsCmd", "dfs", "-test", "-e", s"$hdfsPrefix/${sampleID}-final-sorted-bqsr.bam").!
          if (retFileExists == 0) {
            val retMkdir = Seq("mkdir", s"$dataDirLocal/tmp_$sampleID").!
            retVariantCaller = Seq(s"$gatk", "HaplotypeCallerSpark",
              "-R", s"$dataDir/$referenceGenome.fa",
              "-I", s"$hdfsPrefix/${sampleID}-final-sorted-bqsr.bam",
              "-O", s"$hdfsPrefix/${sampleID}.vcf",
              "--tmp-dir", s"$dataDir/tmp_$sampleID",
              "--", "--spark-runner", "SPARK",
              "--spark-master", "yarn",
              "--conf", "spark.executor.memory=24g").!

            // Delete tmp directory
            val retDel = Seq("rm", "-rf", s"$dataDirLocal/tmp_$sampleID").!
          }
        }
      }
    } catch {
      case e: Exception => print(s"Exception in HaplotypeCaller/Mutect2, check sequence ID $x")
    }

    // Create a empty .retry file
    val retryExt = ".retry"

    val retCreateRetryExt =
      if (retVariantCaller != 0) {Seq(s"$hdfsCmd", "dfs", "-touchz", s"$hdfsPrefix/${sampleID}${retryExt}").!} else {0}

    val endTime = Calendar.getInstance().getTime()

    // Delete all intermediate files as they consume a lot of space
    if (useSomatic == true) {
      val retDelbam = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"$hdfsPrefix/${sampleID}_*-*.bam*").!

      println(s"Completed GATK_Mutect2 on ($x) ended at $endTime; return values $retVariantCaller; " +
        s"delete return values: ${retDelbam} " +
        s"create $retryExt file return value: ${retCreateRetryExt} ")

      println(s"Completed-GATK_Mutect2-$x")
    }
    else {
      val retDelbam = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"$hdfsPrefix/${sampleID}-*.bam*").!

      println(s"Completed GATK_HaplotypeCaller on ($x) ended at $endTime; return values $retVariantCaller; " +
        s"delete return values: ${retDelbam} " +
        s"create $retryExt file return value: ${retCreateRetryExt} ")

      println(s"Completed-GATK_HaplotypeCaller-$x")
    }
    (x, retVariantCaller)
  }

  // ADAM-Cannoli pipeline stages
  // Stage 1: Interleave FASTQ
  def runInterleave[T](x: T):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting Interleave FASTQ on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    var retInterleave = -1
    try {
      // Create interleaved fastq files
      val cannoliSubmit = sys.env("CANNOLI_HOME") + "/bin/cannoli-submit"
      //val sparkMaster = "spark://vm0:7077"
      val hdfsPrefix = "hdfs://vm0:9000"
      retInterleave = Seq(s"$cannoliSubmit", "--master", "yarn", "--", "interleaveFastq",
        s"$hdfsPrefix/${sampleID}_1.fastq.gz",
        s"$hdfsPrefix/${sampleID}_2.fastq.gz",
        s"$hdfsPrefix/${sampleID}.ifq").!
    } catch {
      case e: Exception => print(s"Exception in Interleave FASTQ, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed Interleave FASTQ on ($x) at ${endTime}, return values: $retInterleave")

    (x, retInterleave)
  }

  // Variant analysis
  def runVariantAnalysis[T](x: T, referenceGenome: String, numNodes: Int):T = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting variant analysis on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val VASubmit = sys.env("EVA_HOME") + "/scripts/run_variant_analysis_adam_basic.sh"
    //val sparkMaster = "spark://vm0:7077"

    val useYARN = "y"
    val hdfsPrefix = "hdfs://vm0:9000"
    val retVA = Seq(s"$VASubmit", s"$referenceGenome",
      s"$hdfsPrefix/${sampleID}_1.fastq.gz",
      s"$hdfsPrefix/${sampleID}_2.fastq.gz",
      s"$numNodes",
      s"$sampleID",
      s"$useYARN").!

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed variant analysis on ($x) ended at $endTime; return values $retVA")

    x
  }


  // Stage 2: BWA
  def runBWA[T](x: T, referenceGenome: String):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting BWA on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val cannoliSubmit = sys.env("CANNOLI_HOME") + "/bin/cannoli-submit"
    //val sparkMaster = "spark://vm0:7077"
    val hdfsPrefix = "hdfs://vm0:9000"
    val bwaCmd = sys.env("BWA_HOME") + "/bwa"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"

    var retBWA = -1
    try {
      // Delete $sampleId.bam_* files
      val retDel = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.bam_*")

      val execBWA = Seq(s"$cannoliSubmit", "--master", "yarn", "--", "bwaMem",
        s"$hdfsPrefix/${sampleID}.ifq",
        s"$hdfsPrefix/${sampleID}.bam",
        "-executable",
        s"$bwaCmd",
        "-sample_id",
        "mysample",
        "-index",
        s"file:///mydata/$referenceGenome.fa",
        "-sequence_dictionary",
        s"file:///mydata/$referenceGenome.dict",
        "-single",
        "-add_files")

      retBWA = (retDel #&& execBWA #|| execBWA).!

    }
    catch {
      case e: Exception => print(s"Exception in BWA, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed BWA on ($x) ended at $endTime; return values bwa: $retBWA")

    (x, retBWA)
  }

  // BWAMEM2
  def runBWAMEM2[T](x: T, referenceGenome: String):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting BWA-MEM2 on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val cannoliSubmit = sys.env("CANNOLI_HOME") + "/bin/cannoli-submit"
    //val sparkMaster = "spark://vm0:7077"
    val hdfsPrefix = "hdfs://vm0:9000"
    val bwaCmd = sys.env("BWAMEM2_HOME") + "/bwa-mem2"
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"

    var retBWA = -1
    try {
      // Delete $sampleId.bam_* files
      val retDel = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.bam_*").!

      val retBWA = Seq(s"$cannoliSubmit", "--master", "yarn", "--", "bwaMem2",
        s"$hdfsPrefix/${sampleID}.ifq",
        s"$hdfsPrefix/${sampleID}.bam",
        "-executable",
        s"$bwaCmd",
        "-sample_id",
        "mysample",
        "-index",
        s"file:///mydata/$referenceGenome.fa",
        "-sequence_dictionary",
        s"file:///mydata/$referenceGenome.dict",
        "-single",
        "-add_files").!
    }
    catch {
      case e: Exception => print(s"Exception in BWA-MEM2, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed BWA-MEM2 on ($x) ended at $endTime; return values bwa: $retBWA")

    (x, retBWA)
  }

  // Stage 3: Sort and Mark Duplicates
  def runSortMarkDup[T](x: T, bqsrIndelMode: Any):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting sort/mark duplicates on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val adamSubmit = sys.env("ADAM_HOME") + "/bin/adam-submit"
    //val sparkMaster = "spark://vm0:7077"
    val hdfsPrefix = "hdfs://vm0:9000"

    var retSortDup = -1
    if (bqsrIndelMode==false) {
      try {
        retSortDup = Seq(s"$adamSubmit", "--master", "yarn", "--", "transformAlignments",
          s"$hdfsPrefix/${sampleID}.bam",
          s"$hdfsPrefix/${sampleID}.bam.adam",
          "-mark_duplicate_reads",
          "-sort_by_reference_position_and_index").!
      } catch {
        case e: Exception => print(s"Exception in sort/mark duplicates, check sequence ID $x")
      }
    }
    else  {
      val known_snps_hdfs = hdfsPrefix + "/known_snps"
      val known_indels_hdfs = hdfsPrefix + "/known_indels"

      try {
        retSortDup = Seq(s"$adamSubmit", "--master", "yarn",
          "--num-executors", "3",
          "--executor-memory", "13g",
          "--driver-memory", "13g",
          "--", "transformAlignments",
          s"$hdfsPrefix/${sampleID}.bam",
          s"$hdfsPrefix/${sampleID}.bam.adam",
          "-recalibrate_base_qualities",
          "-known_snps",
          s"$known_snps_hdfs",
          "-realign_indels",
          "-known_indels",
          s"$known_indels_hdfs",
          "-mark_duplicate_reads",
          "-sort_by_reference_position_and_index").!
      } catch {
        case e: Exception => print(s"Exception in sort/mark duplicates/BQSR/Indel realign, check sequence ID $x")
      }
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed sort/mark duplicates on ($x) ended at $endTime; mode: $bqsrIndelMode return values $retSortDup")

    (x, retSortDup)
  }

  // Stage 3: Sort, Mark Duplicates, BSQR, indel realignment
  def runSortMarkDupBQSRIndel[T](x: T):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting sort/mark duplicates/BQSR/Indel realign on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val adamSubmit = sys.env("ADAM_HOME") + "/bin/adam-submit"
    //val sparkMaster = "spark://vm0:7077"
    val hdfsPrefix = "hdfs://vm0:9000"
    val known_snps_hdfs = hdfsPrefix + "/known_snps"
    val known_indels_hdfs = hdfsPrefix + "/known_indels"

    var retSortDupBQSRIndel = -1
    try {
      retSortDupBQSRIndel = Seq(s"$adamSubmit", "--master", "yarn",
        "--num-executors", "5",
        "--executor-memory", "40g",
        "--driver-memory", "40g",
        "--", "transformAlignments",
        s"$hdfsPrefix/${sampleID}.bam",
        s"$hdfsPrefix/${sampleID}.bam.adam",
        "-recalibrate_base_qualities",
        "-known_snps",
        s"$known_snps_hdfs",
        "-realign_indels",
        "-known_indels",
        s"$known_indels_hdfs",
        "-mark_duplicate_reads",
        "-sort_by_reference_position_and_index").!
    } catch {
      case e: Exception => print(s"Exception in sort/mark duplicates/BQSR/Indel realign, check sequence ID $x")
    }

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed sort/mark duplicates/BQSR/Indel realign on ($x) ended at $endTime; return values $retSortDupBQSRIndel")

    (x, retSortDupBQSRIndel)
  }

  // Stage 4: Freebayes
  def runFreebayes[T](x: T, referenceGenome: String):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting Freebayes on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val cannoliSubmit = sys.env("CANNOLI_HOME") + "/bin/cannoli-submit"
    //val sparkMaster = "spark://vm0:7077"
    val hdfsPrefix = "hdfs://vm0:9000"
    val freeBayesCmd = sys.env("FREEBAYES_HOME") + "/bin/freebayes"

    var retFreebayes = -1
    try {
      retFreebayes = Seq(s"$cannoliSubmit", "--master", "yarn", "--", "freebayes",
        s"$hdfsPrefix/${sampleID}.bam.adam",
        s"$hdfsPrefix/${sampleID}.vcf",
        "-executable",
        s"$freeBayesCmd",
        "-reference",
        s"file:///mydata/$referenceGenome.fa",
        "-add_files",
        "-single").!
    } catch {
      case e: Exception => print(s"Exception in Freebayes, check sequence ID $x")
    }

    // Delete all intermediate files as they consume a lot of space
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val retDelifq = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.ifq").!
    val retDelbam = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.bam*").!
    val retDelvcf = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.vcf_*").!

    // Create a empty .retry file
    val retryExt = ".retry"

    val retCreateRetryExt =
      if (retFreebayes != 0) {Seq(s"$hdfsCmd", "dfs", "-touchz", s"/${sampleID}${retryExt}").!} else {0}

    val endTime = Calendar.getInstance().getTime()
    println(s"Completed Freebayes on ($x) ended at $endTime; return values $retFreebayes; " +
      s"delete return values: ${retDelvcf}+${retDelifq}+${retDelbam} " +
      s"create $retryExt file return value: ${retCreateRetryExt} ")

    (x, retFreebayes)
  }

  // Cleanup temporary files before retrying variant analysis
  def cleanupFiles[T](x: T):(T, Int) = {
    val beginTime = Calendar.getInstance().getTime()

    println(s"Starting Cleanup on ($x) at $beginTime")

    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    // Delete all intermediate files
    val hdfsCmd = sys.env("HADOOP_HOME") + "/bin/hdfs"
    val retDelifq = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.ifq").!
    val retDelbam = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.bam*").!
    val retDelvcf = Seq(s"$hdfsCmd", "dfs", "-rm", "-r", "-skipTrash", s"/${sampleID}.vcf*").!

    val finalRes = retDelifq + retDelbam + retDelvcf
    val endTime = Calendar.getInstance().getTime()
    println(s"Completed Cleanup on ($x) ended at $endTime; return value $finalRes; " +
      s"delete return values: ${retDelvcf}+${retDelifq}+${retDelbam}")

    (x, finalRes)
  }

  // Denovo assembly
  def runDenovo[T](x: T, kmerVal: Int):T = {
    val beginTime = Calendar.getInstance().getTime()
    println(s"Starting Abyss on ($x) at $beginTime")
    //val sampleID = x.toString
    val (sampleID, partitionID) = splitSampleInfo(x.toString)

    val cleanUp = "rm -rf /mydata/$sampleID*"
    val cleanRet = Process(cleanUp).!

    // First copy the file from HDFS to /mydata
    val dataDir = "/mydata"
    val copyCmd =
      sys.env("HADOOP_HOME") + "/bin/hdfs dfs -get -f " +
        s" /$sampleID.ifq $dataDir"
    val retCopy = Process(copyCmd).!

    println(s"Completed HDFS copy...")

    // Run Abyss; only interleaved FASTQ works with Scala Process call

    val abyssDir = sys.env("HOMEBREW_PREFIX")
    val cmd =
      s"$abyssDir/bin/abyss-pe j=30 k=$kmerVal -C $dataDir " +
        s" name=$sampleID " +
        s" in=$dataDir/$sampleID.ifq"
    println(cmd)
    val abyssRet = Process(cmd).!

    // Although abyss-pe takes two paired-end files, it fails later inside the script
    //val ret = Seq(s"$abyssDir/abyss-pe", "j=30", "k=71", "-C", s"$dataDir",
    //  s"name=$sampleID", s"in='${sampleID}_1.filt.fastq.gz ${sampleID}_2.filt.fastq.gz'").!

    // Copy .fa to HDFS
    val cmdToCopyFa =
      sys.env("HADOOP_HOME") + "/bin/hdfs dfs -put -f " +
        s" $dataDir/$sampleID-scaffolds.fa /"
    println(cmdToCopyFa)
    val facopyRet = Process(cmdToCopyFa).!
    val endTime = Calendar.getInstance().getTime()
    println(s"Abyss ended at ${endTime}, return values: ", cleanRet, retCopy, abyssRet, facopyRet)

    x
  }
}


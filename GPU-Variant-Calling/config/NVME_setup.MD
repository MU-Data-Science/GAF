# Instructions to attach NVME storage in Fabric
Step: 1 Verify the disk

Step: 2 Partition the disk  
Command: `sudo fdisk /dev/nvme0n1`  
Step: 3 Format the disk  
Command: `sudo mkfs.ext4 /dev/nvme0n1p1`    
Step: 4 Create the mount point  
Command: `sudo mkdir /mnt/nvme`  
Step: 5 Mount the partition  
Command: `sudo mount /dev/nvme0n1p1 /mnt/nvme`  

Detailed information for step 2 mentioned below,
For Step: 2 Partition the disk  
Create a New Partition:

Type n and press Enter to create a new partition.
Select p for a primary partition.
Accept the default partition number by pressing Enter.
Accept the default first sector by pressing Enter.
Accept the default last sector (or specify a size, e.g., +100G) by pressing Enter.
Verify the Partition:

Type p and press Enter to print the partition table. You should see the new partition (e.g., /dev/nvme0n1p1).
Write Changes:

Type w and press Enter to write the changes to the disk and exit fdisk.

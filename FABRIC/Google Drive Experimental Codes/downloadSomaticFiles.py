import gdown 
import os 


print('Enter OAuth 2 Token:')
token = str(input())
print()

##df1
ids = ['1RYaXlxSys5G8dx9XIW-8HnVvfO3jzonV', '1rkqNpsDXVrji9Nx2XDUas1I5jyckJi4j', '1w8Lr0BkntVITSCSTAZNsj2IQOcUtV8lF', '1n8qZKzMAN73KPG_44Kentj3u2K8QvZ2u', '1GNIU0VRewqmBNM6hTzXhVrQIBPs5eC4V', '18dZANabqM62Vt2gw5ju0t7KQ_IUQ4BlW', '1wel5U2Bt-qnpFUJA6jqeHndKJB-dS_p9', '1uX9-3J9Vo54A1TK-JInJnMNUP23qP1E2', '1Dr3ca4R_m7Vv8qb4ZZOufE0JaUftrl_T', '1OJn5yfiMn7jSXjt0wzRHrOlJevZYUB1l', '16plinE-2fHYvPaqnZ9Iouhy3qddATX4r', '1nQjYhezpi9NfGlyZnVd2c5lM2Y_5Nh6F', '1BWA7_hoKpzQNFftlN_B1hZo_oYLAFtJW', '1jOcN0xs6FZeentQt7MedtOGGL7UXaNti', '1z4_Z_PqfHJ_tBvK2RVY5e4dNImzfSDw3', '1hBo6cKCYO1MXV4b58acaMh4fQt0b2ke1', '1Jkkkwixm0cIhRTKfTc5DmWMA_RQpSyzV', '12gWKdiEV1GdScLwVLFFgQMkslQu31wFK', '1mKKWkQJZ7f3-pvPTT7mv-ogjl-NX0-Dh', '1ysAyS9mr2_lMAvU5qK8VwnJvD5HJTwKY']
names = ['TCRBOA5_normal_1.fastq.gz', 'TCRBOA4_normal_1.fastq.gz', 'TCRBOA3_tumor_1.fastq.gz', 'TCRBOA3_normal_1.fastq.gz', 'TCRBOA2_tumor_1.fastq.gz', 'TCRBOA2_normal_1.fastq.gz', 'TCRBOA1_normal_1.fastq.gz', 'TCRBOA5_tumor_2.fastq.gz', 'TCRBOA5_tumor_1.fastq.gz', 'TCRBOA5_normal_2.fastq.gz', 'TCRBOA4_tumor_2.fastq.gz', 'TCRBOA4_tumor_1.fastq.gz', 'TCRBOA4_normal_2.fastq.gz', 'TCRBOA3_tumor_2.fastq.gz', 'TCRBOA3_normal_2.fastq.gz', 'TCRBOA2_tumor_2.fastq.gz', 'TCRBOA2_normal_2.fastq.gz', 'TCRBOA1_tumor_2.fastq.gz', 'TCRBOA1_tumor_1.fastq.gz', 'TCRBOA1_normal_2.fastq.gz']


for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)
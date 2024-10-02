import os 


print('Enter OAuth 2 Token:')
token = str(input())
print()

#fastq files for demo 
names = ['Homo_sapiens_assembly38.known_indels.vcf.gz', 'Homo_sapiens_assembly38.known_indels.vcf.gz.tbi', 'Homo_sapiens_assembly38.dbsnp138.vcf.gz.tbi', 'Homo_sapiens_assembly38.dbsnp138.vcf.gz', 'hs38.fa.img', 'hs38.fa.sa', 'hs38.fa.pac', 'hs38.fa.fai', 'hs38.fa.bwt', 'hs38.fa.ann', 'hs38.fa.amb', 'hs38.fa', 'hs38.dict']
ids = ['1LbLDI5zN-pcFaNFTRAONGH3tbX9F2Sgj', '1CBmnt9ix6aDF6zj0N0f3RTrgXB72kzWT', '16hKDaGmv3Jmdq-MYCA_L-KHvARj1Pgxh', '1yXxQ4nSCYkittNnNJwX5xf1rZy2GMrL6', '1-uRu2pim_2XZw-niFbpVTQ6BpzFZPu4g', '1u80_9WYIsfQquz7qcGmeGf7l6DByammE', '1kHKJ2KJEYkTiBTis1gtZ-3COtZlz_JMD', '14ttgPQApQCYGGDnZ9IyE_4JvxMA5aPR-', '16sWQfRKAajvyQIw873v6HzIQh6dk0GQt', '1bF_f_dubDPk9II2ZMAmDG_EKBVawwPws', '1AhYWNb_C3Ijr5m6rcwSQwqXSYkI2VD-R', '1MGr5ylqE-gb2t3MFnVL0oNcFdncGnQZo', '1I_UHjWLk9Qwg9gQqJ_NWwNh8yPV44i5Z']


for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)

import os 


print('Enter OAuth 2 Token:')
token = str(input())
print()

#fastq files for demo 
names = ['ERR018416_2.fastq.gz', 'ERR018423_1.fastq.gz', 'ERR018423_2fastq.gz', 'ERR018429_1.fastq.gz', 'ERR018429_2.fastq.gz', 'ERR018435_1.fastq.gz', 'ERR018435_2.fastq.gz', 'ERR018436_1.fastq.gz', 'ERR018436_2.fastq.gz', 'ERR018442_1.fastq.gz', 'ERR018442_2.fastq.gz', 'ERR018448_1.fastq.gz', 'ERR018448_2.fastq.gz', 'ERR018454_1.fastq.gz', 'ERR018454_2.fastq.gz', 'ERR018460_1.fastq.gz', 'ERR018460_2.fastq.gz', 'ERR018463_1.fastq.gz', 'ERR018463_2.fastq.gz', 'ERR018469_1.fastq.gz', 'ERR018469_2.fastq.gz', 'ERR018475_1.fastq.gz', 'ERR018475_2.fastq.gz', 'ERR018476_1.fastq.gz', 'ERR018476_2.fastq.gz', 'ERR018482_1.fastq.gz', 'ERR018482_2.fastq.gz']
ids = ['11pb5r5a5wm-im9cXtuwOVvzSj910Jdr2', '11wXExkz23RjQ4oKnzstl4mm_-C_DX8lg', '11y7mnLccUofs0m4LO3vxrfydVVxUe3fi', '12RWQQsXLiCxv6LtPo4smnWPWBmUc_y-_', '12W17ICrbjkSQyTX8zSC9u6euQkfX8lbu', '12B72HUgEeze4hIyl0vog_bEVSo_mW1Uf', '12JAlZ9Jjvl4ZjvahNRStsCypSM_tjbh6', '12KQawFv_CuvSVUCdCSl0f6P1p5aJ54Ce', '12KlB7kDoGISS2113Ml-4YyWWjQaZj4m6', '12chm5sAq8YSwKjgk-7UV5cWeAOvXwXju', '12gV_IDmvIySW0GH_7VQKGmsIpUQZem00', '12psn1p3AWJ2hsxgboVCUSumNefyBkWxf', '12vjhlHzo5k32KgPWHI-Apo6GyhF_bvkS', '13ESRulkZvCSOdGpohmqT5UiT8M20C6Fm', '13Q_KFGNT428RgZqZzks8J6jMGjio-0uW', '13_Sm_ntCPCTNwuIyCBL3eqBgimGSWDje', '13_cmn8Em-m8OYWL9LEYqzYVUc9AkaCiO', '13f7qfwPIajp6FAL5pAMPxl6DzK4XVjk9', '13kfF_PHDfrpKRM9AR0nVx7MmVVD23Ke7', '13u-BdvKq3fJL3xcN8iOojNv_LoCaTU00', '13uiGxhIO9cQyju-xIX9Q0iqziamtnNAW', '14F2nkz6cVRDuSRg_vfawrSnsBiEO6_5H', '14FEL5zdVQRsk2ny6YTmIym8QQKIMyeDK', '14H605OlCpQ_EM5ZTcKrbuPircXghlpIu', '14KzX0UaKV3F1oOhAVI9QrtebRM5rDmRt', '14fhpR7HS-47tyGGHdIg255HjlFIOBw_h', '14lvK1mL2fRiCMyDhh-uNePZAebbvcZ6M']


for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)

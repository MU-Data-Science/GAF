import os 
import re 
import subprocess


print('Enter OAuth 2 Token:')
token = str(input())

ids = ['1-DPbHzxj5hRAPZXvycvipsTPo3pBe0u3', '1-CWPFfXFWiTRMaBztRGo60gnjw1tQR_P', '14qy07EWvh6mUQdtHGofNi39jhGo43thR', '14odJ8xmFKpQTeBDopuX88-7p-hdOMLpT', '151P7oiGlwBFr49t_cz_O6gua8jyP5-QA', '150SN6dyK9CIIzSrh-xnMWaXAqa3h_Gje', '1-FY0SYivx7YJK4-dkC85CILp070_7jbU', '1-EfL72E-zqitBnsN1gooVIvW2uDdLWh0', '1-VUg_qpdPZWwXBkmTH7-IviCjr3kmzaR', '1-RXJfhlmMoFudWi5xNMM2_MaWaq9lqmu', '1-omePzaY3WvfUHy18J7bIzJNrW1d5AaN', '1-jLC-3veiOPR-33gy3mEeCZkiFa-TOna', '1-TAGCgpYbg51ozBzgdhB6xQVUkvTL6UV', '1-NbZIOEBjgeigQPtjWhUUz-0-1FwaPnC', '1-hFwsEcwyOw5CdqPxHCQ4aPWJLwnO7Bv', '1-bcktb4vKUFZSy2nncS0aHLdYjWYLrW3', '10U_gaHk2TVLoYR2yOE3qAORjuHXZfyii', '10SNjyC1VmsvG0mJe0fXB0QjQ96B3Hbl6', '10nq1_fpMqyC2Pmcn1HpzvJPfGWsTF7c6', '10gjWWoOcT__UWrOD4jiR8i5OLWurWL73', '10oVDNjuv3Nr1bakYEdIMp4HevvflUOE_', '10o3hCqhRolVuNQoOhKiDzKoG9MCilahv', '11RfdYH7ux0WoKuLe4cbnFVJ6-WBBo1F1', '11MXXFHSsZgyoK2QXkMfFe5Z9yrhmXJbe', '11UQfjyrOuO57D8CTso-pUzigPGiFQ2PC', '11ZpXCivrJD9x5qDvqBpwx7JtddWblxRv', '11pb5r5a5wm-im9cXtuwOVvzSj910Jdr2', '11lXc3CI8geVENaplWNbPMOby7Zi13hC4', '11y7mnLccUofs0m4LO3vxrfydVVxUe3fi', '11wXExkz23RjQ4oKnzstl4mm_-C_DX8lg', '12W17ICrbjkSQyTX8zSC9u6euQkfX8lbu', '12RWQQsXLiCxv6LtPo4smnWPWBmUc_y-_', '12JAlZ9Jjvl4ZjvahNRStsCypSM_tjbh6', '12B72HUgEeze4hIyl0vog_bEVSo_mW1Uf', '12KlB7kDoGISS2113Ml-4YyWWjQaZj4m6', '12KQawFv_CuvSVUCdCSl0f6P1p5aJ54Ce', '12gV_IDmvIySW0GH_7VQKGmsIpUQZem00', '12chm5sAq8YSwKjgk-7UV5cWeAOvXwXju', '12vjhlHzo5k32KgPWHI-Apo6GyhF_bvkS', '12psn1p3AWJ2hsxgboVCUSumNefyBkWxf', '13Q_KFGNT428RgZqZzks8J6jMGjio-0uW', '13ESRulkZvCSOdGpohmqT5UiT8M20C6Fm', '13_cmn8Em-m8OYWL9LEYqzYVUc9AkaCiO', '13_Sm_ntCPCTNwuIyCBL3eqBgimGSWDje', '13kfF_PHDfrpKRM9AR0nVx7MmVVD23Ke7', '13f7qfwPIajp6FAL5pAMPxl6DzK4XVjk9', '13uiGxhIO9cQyju-xIX9Q0iqziamtnNAW', '13u-BdvKq3fJL3xcN8iOojNv_LoCaTU00']
names = ['ERR016314_2.fastq.gz', 'ERR016314_1.fastq.gz', 'ERR016316_1.fastq.gz', 'ERR016316_2.fastq.gz', 'ERR016317_1.fastq.gz', 'ERR016317_2.fastq.gz', 'ERR016320_2.fastq.gz', 'ERR016320_1.fastq.gz', 'ERR016326_2.fastq.gz', 'ERR016326_1.fastq.gz', 'ERR016327_2.fastq.gz', 'ERR016327_1.fastq.gz', 'ERR016338_2.fastq.gz', 'ERR016338_1.fastq.gz', 'ERR016344_2.fastq.gz', 'ERR016344_1.fastq.gz', 'ERR016350_2.fastq.gz', 'ERR016350_1.fastq.gz', 'ERR018197_2.fastq.gz', 'ERR018197_1.fastq.gz', 'ERR018198_2.fastq.gz', 'ERR018198_1.fastq.gz', 'ERR018204_2.fastq.gz', 'ERR018204_1.fastq.gz', 'ERR018395_1.fastq.gz', 'ERR018395_2.fastq.gz', 'ERR018416_2.fastq.gz', 'ERR018416_1.fastq.gz', 'ERR018423_2.fastq.gz', 'ERR018423_1.fastq.gz', 'ERR018429_2.fastq.gz', 'ERR018429_1.fastq.gz', 'ERR018435_2.fastq.gz', 'ERR018435_1.fastq.gz', 'ERR018436_2.fastq.gz', 'ERR018436_1.fastq.gz', 'ERR018442_2.fastq.gz', 'ERR018442_1.fastq.gz', 'ERR018448_2.fastq.gz', 'ERR018448_1.fastq.gz', 'ERR018454_2.fastq.gz', 'ERR018454_1.fastq.gz', 'ERR018460_2.fastq.gz', 'ERR018460_1.fastq.gz', 'ERR018463_2.fastq.gz', 'ERR018463_1.fastq.gz', 'ERR018469_2.fastq.gz', 'ERR018469_1.fastq.gz']

p1_ids = ids[:16]
p1_names = names[:16]

p2_ids = ids[16:32]
p2_names = names[16:32]

p3_ids = ids[32:]
p3_names = names[32:]

template = """
import os

os.chdir("/gss/fastqFiles/")

token = "{token}"
ids = {ids}
names = {names}

for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s' % (token, file_id, output_file)
    os.system(cmd)
"""

with open("/gss/download1.py", "w") as f:
    f.write(template.format(ids=p1_ids, names=p1_names,token=token))
with open("/gss/download2.py", "w") as f:
    f.write(template.format(ids=p2_ids, names=p2_names,token=token))
with open("/gss/download3.py", "w") as f:
    f.write(template.format(ids=p3_ids, names=p3_names,token=token))
    

print()
for i in range(4,7):
    cmd = """ssh vm%s "screen -dm bash -c 'python3 /gss/download%s.py; exec sh' " """%(i,i-3)
    print("opening screen on vm%s"%i)
    subprocess.run(cmd, shell=True, capture_output=True, text=True)


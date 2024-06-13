import os 


print('Enter OAuth 2 Token:')
token = str(input())
print()

#fastq files for demo 
names = ['ERR016314_1.fastq.gz', 'ERR016314_2.fastq.gz', 'ERR016316_1.fastq.gz', 'ERR016316_2.fastq.gz', 'ERR016317_2.fastq.gz', 'ERR016317_1.fastq.gz', 'ERR016320_1.fastq.gz', 'ERR016320_2.fastq.gz', 'ERR016326_1.fastq.gz', 'ERR016326_2.fastq.gz', 'ERR016327_1.fastq.gz', 'ERR016327_2.fastq.gz', 'ERR016338_1.fastq.gz', 'ERR016338_2.fastq.gz', 'ERR016344_1.fastq.gz', 'ERR016344_2.fastq.gz', 'ERR016350_1.fastq.gz', 'ERR016350_2.fastq.gz', 'ERR018197_1.fastq.gz', 'ERR018197_2.fastq.gz', 'ERR018198_1.fastq.gz', 'ERR018198_2.fastq.gz', 'ERR018204_1.fastq.gz', 'ERR018204_2.fastq.gz', 'ERR018395_1.fastq.gz', 'ERR018395_2.fastq.gz', 'ERR018416_1.fastq.gz']
ids = ['1-CWPFfXFWiTRMaBztRGo60gnjw1tQR_P', '1-DPbHzxj5hRAPZXvycvipsTPo3pBe0u3', '14qy07EWvh6mUQdtHGofNi39jhGo43thR', '14odJ8xmFKpQTeBDopuX88-7p-hdOMLpT', '150SN6dyK9CIIzSrh-xnMWaXAqa3h_Gje', '151P7oiGlwBFr49t_cz_O6gua8jyP5-QA', '1-EfL72E-zqitBnsN1gooVIvW2uDdLWh0', '1-FY0SYivx7YJK4-dkC85CILp070_7jbU', '1-RXJfhlmMoFudWi5xNMM2_MaWaq9lqmu', '1-VUg_qpdPZWwXBkmTH7-IviCjr3kmzaR', '1-jLC-3veiOPR-33gy3mEeCZkiFa-TOna', '1-omePzaY3WvfUHy18J7bIzJNrW1d5AaN', '1-NbZIOEBjgeigQPtjWhUUz-0-1FwaPnC', '1-TAGCgpYbg51ozBzgdhB6xQVUkvTL6UV', '1-bcktb4vKUFZSy2nncS0aHLdYjWYLrW3', '1-hFwsEcwyOw5CdqPxHCQ4aPWJLwnO7Bv', '10SNjyC1VmsvG0mJe0fXB0QjQ96B3Hbl6', '10U_gaHk2TVLoYR2yOE3qAORjuHXZfyii', '10gjWWoOcT__UWrOD4jiR8i5OLWurWL73', '10nq1_fpMqyC2Pmcn1HpzvJPfGWsTF7c6', '10o3hCqhRolVuNQoOhKiDzKoG9MCilahv', '10oVDNjuv3Nr1bakYEdIMp4HevvflUOE_', '11MXXFHSsZgyoK2QXkMfFe5Z9yrhmXJbe', '11RfdYH7ux0WoKuLe4cbnFVJ6-WBBo1F1', '11UQfjyrOuO57D8CTso-pUzigPGiFQ2PC', '11ZpXCivrJD9x5qDvqBpwx7JtddWblxRv', '11lXc3CI8geVENaplWNbPMOby7Zi13hC4']


for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)

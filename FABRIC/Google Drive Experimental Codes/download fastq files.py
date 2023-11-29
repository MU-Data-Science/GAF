import gdown 
import os 


token = "input your auth token"


##df1
ids = ['1-9mCqCBCK5XiDe_KP1JQZEtFbFuWcGHf', '1-6mxml7ButR26uLFdnjsc_Z-YTu3PxRt', '1-GjgPi22idoLvF9cNulQI9NvWhvH5fAS', '15ClT4X9HTFD4qAasMMWAcLiMP7TGwxq1', '15AVGEAQWvnhWIrU1aIPDuq09M2iznbTw', '155l-KBtfH6G8rBRrUREYIYcfqFS-wvX4', '155MISjtXaHB_1UmbrV7xJGSqXWURf0Om', '10far6ndXHSC4QGdpAIIwVcBHGZm0wHCG', '10du3kh7RHigwLR04Kr0rcBfb_rmVZ-dq', '10_ETWuycFPmwmjmK5K1g3MDff6qWoeWn', '10Vdf0rZNPMgu1gNloEXcTzWUu9RhwaPi', '1-q2mvVDteYkVGw150XpV5OQVXjq-7hb5', '1-_6coHEquyUtJ6f7He6WLSqJFH-d9nrX', '1-KUVV381vE09A9GO-MWpOnmNF4gpnm3Z', '121sSzdaS65708wWxeETLP1nSfdbWluMq', '11xNMFk09hrnGMiNrO4apsN0bPtSBxEoi', '1-Jt_kOTTkANZtsbi998Yrd8zdAw7U8MZ', '11tX7Njy13ygMcptobGE3XwU2C-AXG1oT', '10COwULoSN4ymftOjpMa3L27Y5eSk-Fzn', '11p5_GfWyReBrHLdK3D1SKvMJSwztuxGr', '1-sMy7-5a3KAvDWfCuipPVNV930FxD8Kp', '11lYjIhyyHHFyrt8acYaiZ2wD4bngLMND', '11dvWSasfzybmUNsqaP4UCsQ3gDFEJRq6', '11ZVFSFYkO1WGg5wlUfW0qVTxrcqIwn4o', '11VqXK7_flYT2bLLvUmVn5DF1HZc0tapK', '11VMN3hbJwLc__Aj0mFTHvsMCon7He60H', '11NUJXUp-y_2XfIxn3_jmXcy8ByEmLYS6', '11FL5_KBBJa7j49NrCyHEUSyuSK0L1gvp', '11EXVfRD9LUKNROTn7NxtOaTCUTWKwvir', '11A_poAbnCUKjEMewHp1JxwtVBLIazdoO', '10yRzb-BxcEQgcMOhCOAWf1ssALMV9Vmm', '10wFMr5bdIgeZvq87UGv3KTmp7b7evqGl', '10v89S4XGJtgPp-wZnoyswwNJn2nW3Sh1', '1-4DpDJKCMGBnzWNvqcSGVRcMNCArU2tL', '10iDxDQo3X1VFxAT9l9MvybBJgCsEIz96', '10hhVUed8sik-6k6vJ0zF03dw9ADLY34p', '10AWiU0NZ8LdVV3GdePRv_cqz0WMlUBQs', '1-60QCykitMU5J9cqyREByR3x-45d8H5d', '10ASPzWSXIvK_RwqKuAzbmBZ0ybTuRonN', '1-4sXikSYDAJqveFWr5-IvAx986btbQiv', '1-1A63_qrJLhVwQSKUJsUPnKc9DcnCRK-', '103a7TCsPWi0nIvIGhm_-lpthON7plKr7', '1Z7VbKOKGjAMwFdGQEWO0bKnb2lV3r7hT', '1005qpZGiI2Il9duffGkCZh6at6gMc0Ob', '1-zcCTAVA8W_pMBibylwpCIEJuQTQGgIT', '1-wgYtn4KItvZ0dsnHvp1t98uD7sez6oS', '1-f1cJglWPh_j8rCGUBl72G-iEjrf5T69', '1-e2YW_0EkhjU0P4q0F_tRviG23puyRD7', '1-LbaySNAG2coRzJLYfXwXMXQY17vrjJh', '1-KjBh1hOrin0uTCwEUE1e0TSeezyUSNX']
names = ['SRR794367_2.fastq.gz', 'SRR794367_1.fastq.gz', 'ERR065364_2.fastq.gz', 'SRR111943_2.fastq.gz', 'SRR111943_1.fastq.gz', 'SRR799760_2.fastq.gz', 'SRR799760_1.fastq.gz', 'SRR794355_2.fastq.gz', 'SRR794355_1.fastq.gz', 'SRR794349_2.fastq.gz', 'SRR794349_1.fastq.gz', 'ERR068392_2.fastq.gz', 'ERR068392_1.fastq.gz', 'SRR794319_2.fastq.gz', 'ERR062979_2.fastq.gz', 'ERR062979_1.fastq.gz', 'SRR794319_1.fastq.gz', 'SRR794312_2.fastq.gz', 'SRR794397_2.fastq.gz', 'SRR794312_1.fastq.gz', 'SRR794397_1.fastq.gz', 'ERR062973_2.fastq.gz', 'ERR062973_1.fastq.gz', 'ERR062967_2.fastq.gz', 'ERR062967_1.fastq.gz', 'ERR062966_2.fastq.gz', 'ERR062966_1.fastq.gz', 'SRR794398_2.fastq.gz', 'SRR794398_1.fastq.gz', 'ERR062959_2.fastq.gz', 'ERR062959_1.fastq.gz', 'ERR062953_2.fastq.gz', 'ERR062953_1.fastq.gz', 'ERR065364_1.fastq.gz', 'ERR062947_2.fastq.gz', 'ERR062947_1.fastq.gz', 'ERR062941_2.fastq.gz', 'SRR794387_2.fastq.gz', 'ERR062941_1.fastq.gz', 'SRR794387_1.fastq.gz', 'SRR793879_2.fastq.gz', 'SRR794275_2.fastq.gz', 'SRR793879_1.fastq.gz', 'SRR794275_1.fastq.gz', 'ERR062940_2.fastq.gz', 'ERR062940_1.fastq.gz', 'ERR062934_2.fastq.gz', 'ERR062934_1.fastq.gz', 'ERR062930_2.fastq.gz', 'ERR062930_1.fastq.gz']

# for index in range(len(ids)):
#   file_id = ids[index]
#   output_file = names[index]
#   url = f'https://drive.google.com/uc?id={file_id}'
#   gdown.download(url, output_file, quiet=False)

for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)


##df2
ids = ['1-omePzaY3WvfUHy18J7bIzJNrW1d5AaN', '1-jLC-3veiOPR-33gy3mEeCZkiFa-TOna', '1-VUg_qpdPZWwXBkmTH7-IviCjr3kmzaR', '1-RXJfhlmMoFudWi5xNMM2_MaWaq9lqmu', '1-FY0SYivx7YJK4-dkC85CILp070_7jbU', '1-EfL72E-zqitBnsN1gooVIvW2uDdLWh0', '1-DPbHzxj5hRAPZXvycvipsTPo3pBe0u3', '1-CWPFfXFWiTRMaBztRGo60gnjw1tQR_P', '151P7oiGlwBFr49t_cz_O6gua8jyP5-QA', '150SN6dyK9CIIzSrh-xnMWaXAqa3h_Gje', '14qy07EWvh6mUQdtHGofNi39jhGo43thR', '14odJ8xmFKpQTeBDopuX88-7p-hdOMLpT', '14FEL5zdVQRsk2ny6YTmIym8QQKIMyeDK', '14F2nkz6cVRDuSRg_vfawrSnsBiEO6_5H', '13uiGxhIO9cQyju-xIX9Q0iqziamtnNAW', '12W17ICrbjkSQyTX8zSC9u6euQkfX8lbu', '13u-BdvKq3fJL3xcN8iOojNv_LoCaTU00', '12RWQQsXLiCxv6LtPo4smnWPWBmUc_y-_', '13kfF_PHDfrpKRM9AR0nVx7MmVVD23Ke7', '13f7qfwPIajp6FAL5pAMPxl6DzK4XVjk9', '11y7mnLccUofs0m4LO3vxrfydVVxUe3fi', '13_cmn8Em-m8OYWL9LEYqzYVUc9AkaCiO', '11wXExkz23RjQ4oKnzstl4mm_-C_DX8lg', '13_Sm_ntCPCTNwuIyCBL3eqBgimGSWDje', '11pb5r5a5wm-im9cXtuwOVvzSj910Jdr2', '11lXc3CI8geVENaplWNbPMOby7Zi13hC4', '13Q_KFGNT428RgZqZzks8J6jMGjio-0uW', '13ESRulkZvCSOdGpohmqT5UiT8M20C6Fm', '11ZpXCivrJD9x5qDvqBpwx7JtddWblxRv', '11UQfjyrOuO57D8CTso-pUzigPGiFQ2PC', '12vjhlHzo5k32KgPWHI-Apo6GyhF_bvkS', '11RfdYH7ux0WoKuLe4cbnFVJ6-WBBo1F1', '12psn1p3AWJ2hsxgboVCUSumNefyBkWxf', '11MXXFHSsZgyoK2QXkMfFe5Z9yrhmXJbe', '12gV_IDmvIySW0GH_7VQKGmsIpUQZem00', '10oVDNjuv3Nr1bakYEdIMp4HevvflUOE_', '10o3hCqhRolVuNQoOhKiDzKoG9MCilahv', '12chm5sAq8YSwKjgk-7UV5cWeAOvXwXju', '10nq1_fpMqyC2Pmcn1HpzvJPfGWsTF7c6', '10gjWWoOcT__UWrOD4jiR8i5OLWurWL73', '12KlB7kDoGISS2113Ml-4YyWWjQaZj4m6', '12KQawFv_CuvSVUCdCSl0f6P1p5aJ54Ce', '12JAlZ9Jjvl4ZjvahNRStsCypSM_tjbh6', '12B72HUgEeze4hIyl0vog_bEVSo_mW1Uf', '10U_gaHk2TVLoYR2yOE3qAORjuHXZfyii', '10SNjyC1VmsvG0mJe0fXB0QjQ96B3Hbl6', '1-hFwsEcwyOw5CdqPxHCQ4aPWJLwnO7Bv', '1-bcktb4vKUFZSy2nncS0aHLdYjWYLrW3', '1-TAGCgpYbg51ozBzgdhB6xQVUkvTL6UV', '1-NbZIOEBjgeigQPtjWhUUz-0-1FwaPnC']
names = ['ERR016327_2.fastq.gz', 'ERR016327_1.fastq.gz', 'ERR016326_2.fastq.gz', 'ERR016326_1.fastq.gz', 'ERR016320_2.fastq.gz', 'ERR016320_1.fastq.gz', 'ERR016314_2.fastq.gz', 'ERR016314_1.fastq.gz', 'ERR016317_1.fastq.gz', 'ERR016317_2.fastq.gz', 'ERR016316_1.fastq.gz', 'ERR016316_2.fastq.gz', 'ERR018475_2.fastq.gz', 'ERR018475_1.fastq.gz', 'ERR018469_2.fastq.gz', 'ERR018429_2.fastq.gz', 'ERR018469_1.fastq.gz', 'ERR018429_1.fastq.gz', 'ERR018463_2.fastq.gz', 'ERR018463_1.fastq.gz', 'ERR018423_2.fastq.gz', 'ERR018460_2.fastq.gz', 'ERR018423_1.fastq.gz', 'ERR018460_1.fastq.gz', 'ERR018416_2.fastq.gz', 'ERR018416_1.fastq.gz', 'ERR018454_2.fastq.gz', 'ERR018454_1.fastq.gz', 'ERR018395_2.fastq.gz', 'ERR018395_1.fastq.gz', 'ERR018448_2.fastq.gz', 'ERR018204_2.fastq.gz', 'ERR018448_1.fastq.gz', 'ERR018204_1.fastq.gz', 'ERR018442_2.fastq.gz', 'ERR018198_2.fastq.gz', 'ERR018198_1.fastq.gz', 'ERR018442_1.fastq.gz', 'ERR018197_2.fastq.gz', 'ERR018197_1.fastq.gz', 'ERR018436_2.fastq.gz', 'ERR018436_1.fastq.gz', 'ERR018435_2.fastq.gz', 'ERR018435_1.fastq.gz', 'ERR016350_2.fastq.gz', 'ERR016350_1.fastq.gz', 'ERR016344_2.fastq.gz', 'ERR016344_1.fastq.gz', 'ERR016338_2.fastq.gz', 'ERR016338_1.fastq.gz']

# for index in range(len(ids)):
#   file_id = ids[index]
#   output_file = names[index]
#   url = f'https://drive.google.com/uc?id={file_id}'
#   gdown.download(url, output_file, quiet=False)

for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)

##df3
ids = ['1-jKhA0MOKSUSgR9Wusor-H62Q6kvNNZ0', '1-gKK34nVDo3sclkTE6lorGUXXkiT4Ul9', '1-L9XUsIY-lzUh0FVhmj_OzRXApRYfcZX', '1-KSg5GNjW8QplLGF4mjmyMFxz2zoQ0Y0', '1-DvwY3sXPwKerMSutHSOGai5JoyZukyv', '1-Dsttc0pf3Ktr8953ggjwtTc4puSH9iS', '1kNDOH14PFe1cw257fYTrbVfZaFkjte7x', '118j6Vt9PMeu0EDS5kSsO-YXhFnEg_LuU', '10x1unxhHFoPW_a_kZSYtJ6WETAZqta2Z', '10sjZHpaQoL6qu_hvGFMeTtyqapwtWvSV', '10nVZG7KOdlB7o90a2S-s1S3JuMkQVPZ5', '10lVvARF9z-rPn5Mmd2H5wiesYSUF5Hh_', '14lvK1mL2fRiCMyDhh-uNePZAebbvcZ6M', '14fhpR7HS-47tyGGHdIg255HjlFIOBw_h', '14KzX0UaKV3F1oOhAVI9QrtebRM5rDmRt', '14H605OlCpQ_EM5ZTcKrbuPircXghlpIu', '10b0TZto1_8K-OF5Al9-BZZ7RD-8v2Fmr', '10aPcbkFACqAX00t3oduuizNpfWby8aQ6', '10RLU_juMREf7wZbTnpc4Ya3r_IQvMsoN', '10MPf3nj2mvSAsJOeOQlRWpvOxqCYF9vH', '10FSG-OTxIr_SOY0Sk9FBAsmJFUnScAoF', '10CpFgXmsNldH7D8hhtyyRXGEihb-uQb5', '10CT2BXjwa3AHEKLwg91wIRrfM7Jwj1R9', '105Ex03eIYjz1YFO_KQorkAUIp3-Re-Di', '105Ed_ztc6SET02c8nhFgDgsQDpBLRgkC', '10-gFEe8vXFdvVQwnZQFi2DY4uzFnfzSn', '1-v8DkJUAJf6kzi-lX6-U2J_tUY6O6RO1', '1-tKjfRpji7LMlzS4HpJqR-l5k3aJi0ee', '1-NObXhvYL3dBp3PgOKtFQd4DZkGmhFJZ', '1-LwrAhqHHN1T9qNsQ7viLVwFvWU4ZiyC', '11oP4IPLvvftQoAMMvP57Yn0BkEX_ZZNH', '11nzQVqFDHqaoG3OHwq5xo0COWVY8pMuD', '11ZrFPSDMAQ7tTRVyVAfppkkBnPZHedZj', '11ZlS2aYPW32kqBPuQ1Fz5rbbs60fwhCa', '11G4dKTq8AbFrn9MLfo3oT3SnfQprUCke', '11G-bNDMpTPMEO4Nq5IyKSaRsf8dnQhKr', '10uRRJ0RvBPp7UMv4Jo_0tOo1XoAbnnOU', '10u-XoNFAQiMnS5sZz4iKKj-hyk3Z6Q6m', '1-0NtSs2yaUrn89U-29a5OLIKWJfNWRpH', '10ekNQgZcPRhu6Jv8FfaWOvBf49NUsVPy', '10J98W3KvCoCseyjIWyvHBWsPzweXt2Bx', '1mJNa3XbCqXYq5AH0xng9-b_6JhfWuqhx', '1-ioFkJ-Fke6CDAArX_dK7v1LeJdg12Jd', '1-fKbK-u7nXXQTEJGUgfTpACvIdYzzlbR', '1-bP0okAvpwrCD2YqlPEB3-JdOqTJXrG2', '1-X0g-IAVdcjrx2mUYfw6yCQRE8fick0u', '1-ImjTPSwcHVKrH4dKjn53fR5BQ2GW-oM', '1-IXVUuYWSNY4vpTDneN0IbjyiI0n2yQ7', '1-IKqwVrQoX3Gt7gzrnGn8rDbHMt9pV49', '1-D4_7z7xwqto05fEnfM9y97D2q_ljT_U']
names = ['ERR020253_2.fastq.gz', 'ERR020253_1.fastq.gz', 'ERR020247_2.fastq.gz', 'ERR020247_1.fastq.gz', 'ERR020241_2.fastq.gz', 'ERR020241_1.fastq.gz', 'ERR020235_2.fastq.gz', 'ERR020235_1.fastq.gz', 'ERR020229_2.fastq.gz', 'ERR020229_1.fastq.gz', 'ERR020157_2.fastq.gz', 'ERR020157_1.fastq.gz', 'ERR018482_2.fastq.gz', 'ERR018482_1.fastq.gz', 'ERR018476_2.fastq.gz', 'ERR018476_1.fastq.gz', 'ERR019903_2.fastq.gz', 'ERR019903_1.fastq.gz', 'ERR019897_2.fastq.gz', 'ERR019897_1.fastq.gz', 'ERR019498_2.fastq.gz', 'ERR019498_1.fastq.gz', 'ERR019492_2.fastq.gz', 'ERR019492_1.fastq.gz', 'ERR019488_2.fastq.gz', 'ERR019488_1.fastq.gz', 'ERR019482_2.fastq.gz', 'ERR019482_1.fastq.gz', 'ERR019481_2.fastq.gz', 'ERR019481_1.fastq.gz', 'ERR018550_2.fastq.gz', 'ERR018550_1.fastq.gz', 'ERR018544_2.fastq.gz', 'ERR018544_1.fastq.gz', 'ERR018538_2.fastq.gz', 'ERR018538_1.fastq.gz', 'ERR018517_2.fastq.gz', 'ERR018517_1.fastq.gz', 'ERR018558_2.fastq.gz', 'ERR018513_2.fastq.gz', 'ERR018513_1.fastq.gz', 'ERR018558_1.fastq.gz', 'ERR018507_2.fastq.gz', 'ERR018507_1.fastq.gz', 'ERR018501_2.fastq.gz', 'ERR018501_1.fastq.gz', 'ERR018492_2.fastq.gz', 'ERR018492_1.fastq.gz', 'ERR018491_2.fastq.gz', 'ERR018491_1.fastq.gz']

# for index in range(len(ids)):
#   file_id = ids[index]
#   output_file = names[index]
#   url = f'https://drive.google.com/uc?id={file_id}'
#   gdown.download(url, output_file, quiet=False)

for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)

##df4
ids = ['14mjkQfr9pu-oLz0Wmja67wuOkbfx6LDl', '14lyDoeh7t0fxXXRImEMtzuthj_TjL6Rn', '14Pz8_4YG2Pcrj2N1qgHjrg9stFuusiUv', '14N09tAUu2XkEWyOfxbfR9HvXhy_i-osl', '149_0f6fXZLai25KlAj999MGQK_QBD1CA', '147MNIfD67yyH64NGTJ8_C2T8R_9ykzbA', '144oSWE41rHmTVygtWpjjesiv9zewJl9r', '12bigE_GQrc-rm6NTKnwT_b-6uo0qyYXM', '143QjFIhJMsqBIsAu_0t_vzsIXWu8HmRw', '12WYwfm2HreZE19jDSjbTacvIOigCzap8', '13rPi8_Vwk4FTW3E5YwJpwoCqW6W3l1n0', '12GVJ303V8vTR294OpC4UzH-K9HFExoh_', '12AQ4-8_bPYEavMFY_Z_Pxs_L9ga_ewjR', '13pVCBGFL5_DQ9jbUBYErh4Nl1ee48V-4', '12AJYd1cDFOd0DVB85pTonLrF6qxG1i7m', '129hwCR9Q1pTJkkfBJij0HoJ1mk_pnsFk', '13_HS0L3Zo_2UsncPBaFFJbleJjguKP3f', '13Ui6fYCbDf8YZjgOcb83oXXPXe0DzYtn', '11lD5ltsNab94RVX1EpDaHIds-gwKSfNP', '11f2NxG9V2a2y6qVmWF8DDBg_JP20o0Fh', '13BghDy4lnQv_cOHi75kYiXjHaEAW8vrF', '137wSJ0whPN3ezr8cLGLMNhA1BYVAw3c8', '11SWB0bKCPS6-G3Em86_BsepR0K5ogAFw', '136xKqfNLXo_KLKjsxYh7Zbk-eluDgDcu', '12z7976SHICaMJlPLcXHndZV8Tg3vFU2W', '11Rofc_iyrxCV1aUbht3mvnrca_GXutOG', '112CkFq06Z6mb1IDMzzl6V7BiuXeDWwXh', '12oFThK5_Lb-o-EmMzTAR1oGToe7tsrDh', '12lHP5MHo9le_2IrF1bJCgMIAoppQtLjL', '10ycTq52CT9A4mZogmoJDIk0LNAjMafI3', '12YJBh8aThIo8M5SDhmFC1zVkAVMJGfzK', '12Ox2PXSy3wOoqvIUwC5X3NaBS_kDwxLN', '10ead1zi6B0VFjTax7HpoIYF0g245cIBo', '10ckbCIGX0Eud0r7zS46rv1bZv5bY6_cF', '10RgLbW3UnG9SO0VyefAJku-SJT4qWuH7', '10QL5la5R_NI9iN3BnHUvAV-d25EYhGpQ', '1-s9UqEJRG_3SiRtgUleJ_pAvHqh6ty8h', '1-kWBCERWAJrbOEjGzZwkRtPJQCSRL65J', '1-_zqJ5PD6ep6dwLkcD2NLHMR2CoUFP5i', '1-Z7CbTkwfv5vnfwt1WfBvWWc3zLdqrft', '1-Ci97en8XopVjTpt6jPvWnKqTQ684Abt', '1-7eXzrPUepbKK2VeL6NhGabYwlVmmd5q', '1-2O8m06T9Fl9OF_Kb3L9mUnS6vHWCQYD', '1WkSQhRldpGZ3sE6RV1FpsVzb5u0sBv52']
names = ['ERR023221_2.fastq.gz', 'ERR023221_1.fastq.gz', 'ERR023220_2.fastq.gz', 'ERR023220_1.fastq.gz', 'ERR023211_2.fastq.gz', 'ERR023211_1.fastq.gz', 'ERR023210_2.fastq.gz', 'ERR022429_2.fastq.gz', 'ERR023210_1.fastq.gz', 'ERR022429_1.fastq.gz', 'ERR023204_2.fastq.gz', 'ERR022393_2.fastq.gz', 'ERR022393_1.fastq.gz', 'ERR023204_1.fastq.gz', 'ERR022370_2.fastq.gz', 'ERR022370_1.fastq.gz', 'ERR022469_2.fastq.gz', 'ERR022469_1.fastq.gz', 'ERR020289_2.fastq.gz', 'ERR020289_1.fastq.gz', 'ERR022463_2.fastq.gz', 'ERR022463_1.fastq.gz', 'ERR020288_2.fastq.gz', 'ERR022457_2.fastq.gz', 'ERR022457_1.fastq.gz', 'ERR020288_1.fastq.gz', 'ERR020282_2.fastq.gz', 'ERR022456_2.fastq.gz', 'ERR022456_1.fastq.gz', 'ERR020282_1.fastq.gz', 'ERR022450_2.fastq.gz', 'ERR022450_1.fastq.gz', 'ERR020276_2.fastq.gz', 'ERR020276_1.fastq.gz', 'ERR020270_2.fastq.gz', 'ERR020270_1.fastq.gz', 'ERR020264_2.fastq.gz', 'ERR020264_1.fastq.gz', 'ERR020263_2.fastq.gz', 'ERR020263_1.fastq.gz', 'ERR020257_2.fastq.gz', 'ERR020257_1.fastq.gz', 'ERR062924_2.fastq.gz', 'ERR062924_1.fastq.gz']

# for index in range(len(ids)):
#   file_id = ids[index]
#   output_file = names[index]
#   url = f'https://drive.google.com/uc?id={file_id}'
#   gdown.download(url, output_file, quiet=False)

for index in range(len(ids)):
    file_id = ids[index]
    output_file = names[index]
    cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
    os.system(cmd)
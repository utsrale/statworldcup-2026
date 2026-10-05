import docx

doc = docx.Document('Laporan_TBP_SIM.docx')
printing = False
for p in doc.paragraphs:
    if 'PENGEMBANGAN & DESAIN APLIKASI' in p.text:
        printing = True
    if 'IMPLEMENTASI & PEMBAHASAN FITUR' in p.text:
        break
    if printing and p.text.strip():
        print(f"[{p.style.name}] {p.text}")

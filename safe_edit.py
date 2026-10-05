import docx
import re

doc = docx.Document('Laporan_TBP_SIM.docx')

replacements = {
    # 1. Update the title of the contributor section
    r"Daftar Kontributor$": "Daftar Kontributor (Development Team)",
    
    # 2. Merge names and roles (Since docx paragraphs are read one by one, we'll do something a bit manual here)
    # But wait, it's easier to just wipe the text of the role paragraphs and append it to the name paragraphs.
}

def replace_in_runs(paragraph, pattern, replacement):
    full_text = paragraph.text
    if re.search(pattern, full_text):
        paragraph.text = re.sub(pattern, replacement, full_text)

for p in doc.paragraphs:
    for pat, repl in replacements.items():
        replace_in_runs(p, pat, repl)

# Manual fixing for the contributor list
for i in range(len(doc.paragraphs) - 1):
    p1 = doc.paragraphs[i]
    p2 = doc.paragraphs[i+1]
    
    # Example: "Ahmad Zaakiy Hidayat \u2013 M0724019" and "Peran: Pengembangan UI/UX..."
    if "\u2013 M07" in p1.text and "Peran:" in p2.text:
        # Merge them
        p1.text = p1.text.strip() + " (" + p2.text.strip() + ")"
        # Wipe out the second paragraph so it doesn't show up, but don't delete the element to avoid corrupting XML structure
        p2.text = ""
        
# 3. Add Deployment Link
for i, p in enumerate(doc.paragraphs):
    if "Tautan Aplikasi (Deployment Link)" in p.text:
        if i + 1 < len(doc.paragraphs) and "shinyapps.io" not in doc.paragraphs[i+1].text:
            new_p = doc.paragraphs[i+1].insert_paragraph_before("Aplikasi telah berhasil di-deploy dan dapat diakses melalui tautan resmi berikut: https://pncraura.shinyapps.io/StatWorldCup/")
            new_p.style = 'Normal'
            break

doc.save('Laporan_TBP_SIM.docx')
print("Successfully edited.")

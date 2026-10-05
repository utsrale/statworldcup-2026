import docx

def force_repair(filepath):
    doc = docx.Document(filepath)
    
    kontrib_names = [
        "Ahmad Zaakiy Hidayat \u2013 M0724019 (Peran: Pengembangan UI/UX, Desain Glassmorphism, dan Styling CSS)",
        "Fikri Adhiatma Nugroho \u2013 M0724027 (Peran: Manajemen Data, Data Wrangling Klasemen, dan Tabel Reaktif)",
        "Michael Petra Pakpahan \u2013 M0724065 (Peran: Penyusunan Laporan, Pengujian Sistem (Debugging), dan Quality Assurance)",
        "Pancar Aura Zaki Ardika \u2013 M0724071 (Peran: Pengembangan Server-side, Integrasi API football-data.org, dan Logika AI)",
        "Naufal Fadhillah Pellu \u2013 M0724077 (Peran: Algoritma Simulasi Monte Carlo dan Analisis Probabilitas)"
    ]
    
    # Find the Heading "RANCANGAN SISTEM & METODOLOGI"
    target_idx = -1
    for i, p in enumerate(doc.paragraphs):
        if "RANCANGAN SISTEM" in p.text:
            target_idx = i
            break
            
    if target_idx != -1:
        # Delete all paragraphs between "Daftar Kontributor" and this heading
        start_idx = -1
        for i in range(target_idx - 1, -1, -1):
            if "Kontributor" in doc.paragraphs[i].text:
                start_idx = i
                break
                
        if start_idx != -1:
            doc.paragraphs[start_idx].text = "Daftar Kontributor (Development Team)"
            
            # Remove all paragraphs between start_idx and target_idx
            # We do this carefully by working on the XML elements
            for i in range(start_idx + 1, target_idx):
                p = doc.paragraphs[i]
                p._element.getparent().remove(p._element)
                
            # Now insert the new paragraphs BEFORE target_idx
            # Since we removed elements from the tree, doc.paragraphs is stale!
            # We must use doc.paragraphs[target_idx] which still points to the heading element (hopefully)
            target_p = doc.paragraphs[target_idx]
            for name in kontrib_names:
                new_p = target_p.insert_paragraph_before(name)
                new_p.style = 'List Paragraph'
                
    doc.save(filepath)
    print("Done force repair!")

try:
    force_repair('Laporan_TBP_SIM.docx')
except Exception as e:
    print("Error:", e)

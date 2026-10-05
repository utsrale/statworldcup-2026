import docx

def edit_report(filepath):
    doc = docx.Document(filepath)
    
    # Track indices to delete
    delete_indices = []
    
    in_kontributor = False
    kontrib_names = [
        "Ahmad Zaakiy Hidayat \u2013 M0724019 (Peran: Pengembangan UI/UX, Desain Glassmorphism, dan Styling CSS)",
        "Fikri Adhiatma Nugroho \u2013 M0724027 (Peran: Manajemen Data, Data Wrangling Klasemen, dan Tabel Reaktif)",
        "Michael Petra Pakpahan \u2013 M0724065 (Peran: Penyusunan Laporan, Pengujian Sistem (Debugging), dan Quality Assurance)",
        "Pancar Aura Zaki Ardika \u2013 M0724071 (Peran: Pengembangan Server-side, Integrasi API football-data.org, dan Logika AI)",
        "Naufal Fadhillah Pellu \u2013 M0724077 (Peran: Algoritma Simulasi Monte Carlo dan Analisis Probabilitas)"
    ]
    
    for i, p in enumerate(doc.paragraphs):
        text = p.text.strip()
        
        # 1. Kontributor
        if "Daftar Kontributor" in text and "Development Team" not in text:
            p.text = "Daftar Kontributor (Development Team)"
            # clear the next 10 lines and insert new format
            idx = i + 1
            added = False
            while idx < len(doc.paragraphs) and not doc.paragraphs[idx].style.name.startswith('Heading'):
                if doc.paragraphs[idx].text.strip():
                    if not added:
                        # Replace the first few paragraphs with our formatted strings
                        for k_text in kontrib_names:
                            new_p = p.insert_paragraph_before(k_text)
                            new_p.style = 'Normal'
                        added = True
                    delete_indices.append(idx)
                idx += 1
                
        # 2. Deskripsi Fungsional (Inject data source details)
        if "Deskripsi Fungsional Perangkat Lunak" in text:
            # Insert details right after
            new_p = doc.paragraphs[i+1].insert_paragraph_before("Aplikasi ini menggunakan sumber data dari API football-data.org. Variabel-variabel yang digunakan meliputi data klasemen (standings), jadwal (matches), dan daftar pencetak gol (scorers). Periode data yang digunakan adalah data live selama turnamen Piala Dunia 2026 berlangsung.")
            new_p.style = 'Normal'
            
        # 3. Tautan Aplikasi
        if "Tautan Aplikasi (Deployment Link)" in text:
            # add link if missing
            next_p = doc.paragraphs[i+1].text.strip()
            if "shinyapps.io" not in next_p:
                new_p = doc.paragraphs[i+1].insert_paragraph_before("Aplikasi telah berhasil di-deploy dan dapat diakses melalui tautan resmi berikut: https://pncraura.shinyapps.io/StatWorldCup/")
                new_p.style = 'Normal'
                
        # 4. Daftar Pustaka (Hanging indent)
        if "Wickham, H." in text or "Chang, W." in text or "Attali, D." in text or "Football-Data.org." in text or "TokenRouter." in text:
            # Ensure it is a single paragraph and has proper style
            p.style = 'Normal'

    # Remove paragraphs slated for deletion (do it in reverse order)
    for idx in sorted(delete_indices, reverse=True):
        p = doc.paragraphs[idx]
        p._element.getparent().remove(p._element)
        
    doc.save('Laporan_TBP_SIM.docx')
    print("Report edited successfully!")

edit_report('Laporan_TBP_SIM.docx')

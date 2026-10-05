import docx

def safe_replace_ui(filepath):
    doc = docx.Document(filepath)
    
    start_idx = -1
    end_idx = -1
    
    for i, p in enumerate(doc.paragraphs):
        if 'Rancangan Antarmuka (User Interface)' in p.text:
            start_idx = i
        if 'Rancangan Server' in p.text:
            end_idx = i
            break
            
    if start_idx != -1 and end_idx != -1:
        new_text = [
            "Desain antarmuka (UI) dikembangkan menggunakan kerangka kerja pustaka bslib untuk menciptakan tata letak web modern yang responsif. Berikut adalah penjelasan skrip/kode penyusun fitur-fitur utamanya:",
            "1. Struktur Utama Aplikasi: Dibangun menggunakan fungsi page_navbar() dengan tema bs_theme(preset = 'cyborg') untuk menghasilkan palet warna dark mode. Navigasi antar halaman (tab) dideklarasikan menggunakan nav_panel().",
            "2. Landing Page: Menggunakan injeksi fungsi HTML() dan tag elemen murni (tags$div, tags$h1) yang dikombinasikan dengan kelas CSS kustom di www/custom.css untuk menghasilkan efek latar belakang gambar full-screen dan animasi panel glassmorphism.",
            "3. Beranda (Dashboard): Tata letak grid responsif diatur menggunakan fungsi layout_columns(). Modul metrik ringkasan (Top Scorer, Total Match) dibangun dengan fungsi value_box(), sedangkan visualisasi data tabel menggunakan fungsi DTOutput() dari library DT.",
            "4. Stadion & Cuaca: Menggunakan skrip fungsi leafletOutput() pada UI untuk menyediakan ruang render bagi peta geografis interaktif dari library leaflet.",
            "5. Prediksi Monte Carlo: Komponen input pengguna dibangun menggunakan fungsi selectInput() untuk mengatur iterasi (200x, 500x, 1000x). Tombol pemroses menggunakan actionButton() yang dihubungkan dengan library shinyjs (useShinyjs()) untuk menonaktifkan tombol (disabled state) saat komputasi berlangsung.",
            "6. Asisten AI & Informasi Kelompok: Menggunakan kombinasi komponen UI card(), card_header(), dan card_body() untuk menciptakan batas visual yang rapi dan memisahkan antar segmen antarmuka secara elegan."
        ]
        
        # Calculate how many slots we have
        slots = end_idx - start_idx - 1
        
        # If we have enough slots, just overwrite them
        for i in range(len(new_text)):
            if i < slots:
                doc.paragraphs[start_idx + 1 + i].text = new_text[i]
                if i > 0:
                    doc.paragraphs[start_idx + 1 + i].style = 'List Paragraph'
                else:
                    doc.paragraphs[start_idx + 1 + i].style = 'Normal'
            else:
                # If we need more slots, insert them before end_idx
                new_p = doc.paragraphs[end_idx].insert_paragraph_before(new_text[i])
                if i > 0:
                    new_p.style = 'List Paragraph'
                else:
                    new_p.style = 'Normal'
                    
        # If we have too many slots, clear the extra ones
        if slots > len(new_text):
            for i in range(start_idx + 1 + len(new_text), end_idx):
                # Clear text and style to hide it completely
                doc.paragraphs[i].text = ""
                doc.paragraphs[i].style = 'Normal'
                
        doc.save(filepath)
        print("UI section safely repaired")
        
safe_replace_ui('Laporan_TBP_SIM.docx')

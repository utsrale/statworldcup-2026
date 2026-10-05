import docx
import re

doc = docx.Document('Laporan_TBP_SIM.docx')

old_intro = "Desain antarmuka (UI) dirancang menggunakan prinsip modern web design (Glassmorphism, palet warna dark mode elegan dengan aksen neon). Menu utama terdiri dari:"

new_text = [
    "Desain antarmuka (UI) dikembangkan menggunakan kerangka kerja pustaka bslib untuk menciptakan tata letak web modern yang responsif. Berikut adalah penjelasan skrip/kode penyusun fitur-fitur utamanya:",
    "1. Struktur Utama Aplikasi: Dibangun menggunakan fungsi page_navbar() dengan tema bs_theme(preset = 'cyborg') untuk menghasilkan palet warna dark mode. Navigasi antar halaman (tab) dideklarasikan menggunakan nav_panel().",
    "2. Landing Page: Menggunakan injeksi fungsi HTML() dan tag elemen murni (tags$div, tags$h1) yang dikombinasikan dengan kelas CSS kustom di www/custom.css untuk menghasilkan efek latar belakang gambar full-screen dan animasi panel glassmorphism.",
    "3. Beranda (Dashboard): Tata letak grid responsif diatur menggunakan fungsi layout_columns(). Modul metrik ringkasan (Top Scorer, Total Match) dibangun dengan fungsi value_box(), sedangkan visualisasi data tabel menggunakan fungsi DTOutput() dari library DT.",
    "4. Stadion & Cuaca: Menggunakan skrip fungsi leafletOutput() pada UI untuk menyediakan ruang render bagi peta geografis interaktif dari library leaflet.",
    "5. Prediksi Monte Carlo: Komponen input pengguna dibangun menggunakan fungsi selectInput() untuk mengatur iterasi (200x, 500x, 1000x). Tombol pemroses menggunakan actionButton() yang dihubungkan dengan library shinyjs (useShinyjs()) untuk menonaktifkan tombol (disabled state) saat komputasi berlangsung.",
    "6. Asisten AI & Informasi Kelompok: Menggunakan kombinasi komponen UI card(), card_header(), dan card_body() untuk menciptakan batas visual yang rapi dan memisahkan antar segmen antarmuka secara elegan."
]

start_idx = -1
for i, p in enumerate(doc.paragraphs):
    if old_intro in p.text:
        start_idx = i
        break

if start_idx != -1:
    # We want to replace the intro and the following 5 bullet points.
    # So we'll wipe out the next 5 paragraphs that correspond to the old bullets.
    for i in range(start_idx, start_idx + 6):
        if i < len(doc.paragraphs):
            doc.paragraphs[i].text = "" # Wipe out old text
            
    # Now insert the new paragraphs right where the old intro was
    # We can just put them in the current start_idx paragraph, separated by newlines
    # Or insert proper paragraphs
    target_p = doc.paragraphs[start_idx]
    
    # Fill the wiped target_p with the intro
    target_p.text = new_text[0]
    target_p.style = 'Normal'
    
    # Insert the rest as List Paragraphs
    for text in reversed(new_text[1:]): # Reversed because we insert_before or insert_after? We'll insert after.
        # Python-docx doesn't easily do insert_after without raw XML. 
        # But we can use insert_paragraph_before on the NEXT element.
        pass
        
    for text in new_text[1:]:
        new_p = doc.paragraphs[start_idx+1].insert_paragraph_before(text)
        new_p.style = 'List Paragraph'

    doc.save('Laporan_TBP_SIM.docx')
    print("Success replacing UI section")
else:
    print("Could not find the target text!")

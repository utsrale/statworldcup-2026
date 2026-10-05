# StatWorldCup 2026 🏆

> **FIFA World Cup 2026 Prediction & Data Intelligence Hub**  
> Proyek *Team Based Project (TBP)* - Sistem Informasi Manajemen (Kelas F, Kelompok 4)

---

## 📌 Deskripsi Proyek
**StatWorldCup 2026** adalah aplikasi dasbor analitik interaktif berbasis **R-Shiny** yang dirancang khusus untuk menyajikan data turnamen FIFA World Cup 2026 (Amerika Serikat, Kanada, dan Meksiko). Aplikasi ini menggabungkan pelacakan data pertandingan real-time, simulasi prediksi juara berbasis Monte Carlo, informasi cuaca geospasial stadion, dan asisten AI interaktif.

---

## ✨ Fitur Utama

1. **🏟️ Dashboard Utama (Main Hub)**
   - **Hero Match Card**: Menampilkan pertandingan yang sedang berlangsung (*LIVE*), hasil terakhir (*FINISHED*), atau laga terdekat (*SCHEDULED*) lengkap dengan countdown waktu dan crest tim.
   - **Hasil & Jadwal 24 Jam**: Tabel dinamis hasil pertandingan 24 jam terakhir dan jadwal 24 jam mendatang dalam Waktu Indonesia Barat (WIB).
   - **Klasemen Grup Interaktif**: Filter klasemen grup A–H dengan metrik lengkap (Poin, GF, GA, GD, Form).
   - **Statistik Cepat**: Ringkasan total gol turnamen, rata-rata gol/laga, dan status matchday.

2. **🌦️ Stadion & Cuaca Live**
   - **Leaflet Map**: Peta interaktif gelap (*CartoDB.DarkMatter*) memetakan 16 stadion penyelenggara di AS, Kanada, dan Meksiko.
   - **Open-Meteo Integration**: Prakiraan cuaca real-time (suhu, kelembapan, kecepatan angin, kondisi cuaca) untuk setiap stadion yang dipilih.
   - **Daftar Stadion**: Informasi kota, negara, dan kapasitas stadion.

3. **🎲 Predictor Hub (Monte Carlo Simulation)**
   - **Simulasi Dinamis On-Demand**: Pilihan iterasi simulasi (200x, 500x, atau 1000x).
   - **Pemodelan Statistik**: Menggabungkan distribusi *Poisson*, *Bayesian smoothing*, dan *Strength-of-Schedule*.
   - **Top 15 Peluang Juara**: Grafik bar horizontal interaktif berbasis **Plotly**.
   - **Insight Cards**: Sorotan otomatis untuk *Favorit Juara*, *Dark Horse*, dan *Raja Fase Grup*.
   - **Tabel Probabilitas Lengkap**: Peluang setiap tim mencapai babak 32 Besar, 16 Besar, Perempat Final, Semifinal, Final, hingga Juara.

4. **🤖 AI Assistant (MiniMax-M3)**
   - Chatbot interaktif bertenaga AI melalui API **TokenRouter** (model MiniMax-M3).
   - Menyediakan jawaban cerdas dalam Bahasa Indonesia yang kontekstual terhadap jadwal, statistik top scorer, dan klasemen terbaru.
   - Tombol pertanyaan cepat (*Quick Questions*) untuk navigasi instan.

---

## 🛠️ Tech Stack & Library

- **Framework**: [R-Shiny](https://shiny.posit.co/)
- **UI & Styling**: `shinyjs`, `bslib`, Google Fonts (Montserrat), Material Symbols, Custom CSS (Glassmorphism & Dark Mode)
- **Visualisasi Data**: `plotly`, `leaflet`, `DT` (DataTables)
- **Data Manipulation**: `dplyr`, `tidyr`, `purrr`, `lubridate`, `stringr`
- **Caching & Networking**: `cachem`, `memoise`, `httr2`, `jsonlite`, `digest`
- **APIs**:
  - [football-data.org](https://www.football-data.org/) (Data Turnamen)
  - [Open-Meteo](https://open-meteo.com/) (Prakiraan Cuaca Real-time)
  - [TokenRouter](https://tokenrouter.com/) (AI Chatbot MiniMax-M3)

---

## 👥 Tim Pengembang (Kelompok 4)
* **Ahmad Zaakiy Hidayat** (M0724019)
* **Fikri Adhiatma Nugroho** (M0724027)
* **Michael Petra Pakpahan** (M0724065)
* **Pancar Aura Zaki Ardika** (M0724071)
* **Naufal Fadhillah Pellu** (M0724077)

---

## 🚀 Cara Menjalankan Secara Lokal

1. Pastikan R dan pustaka yang dibutuhkan sudah terpasang:
   ```R
   install.packages(c("shiny", "shinyjs", "bslib", "httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr", "lubridate", "DT", "plotly", "leaflet", "readr", "memoise", "cachem", "digest"))
   ```

2. Konfigurasikan file `.Renviron` di dalam folder `SIM_TBP_Skrip_F_Kel4/` dengan API Key:
   ```env
   FOOTBALL_DATA_KEY_1=your_football_data_api_key
   TOKENROUTER_API_KEY=your_tokenrouter_api_key
   ```

3. Jalankan aplikasi melalui RStudio atau konsol R:
   ```R
   shiny::runApp("SIM_TBP_Skrip_F_Kel4")
   ```

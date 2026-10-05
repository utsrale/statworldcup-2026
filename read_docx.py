import docx
import sys

def summarize_docx(filepath):
    try:
        doc = docx.Document(filepath)
        print(f"Summary for {filepath}:")
        for i, p in enumerate(doc.paragraphs):
            if p.text.strip():
                print(f"[{p.style.name}] {p.text.strip()[:100]}")
            if i > 50:
                print("... (truncated)")
                break
        print("-" * 40)
    except Exception as e:
        print(f"Error reading {filepath}: {e}")

summarize_docx("5a. Template Laporan SIM 2025B.docx")
summarize_docx("Laporan_TBP_SIM.docx")

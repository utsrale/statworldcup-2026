import zipfile
import os
import shutil

template_path = '5a. Template Laporan SIM 2025B.docx'
report_path = 'Laporan_TBP_SIM.docx'
output_path = 'Laporan_TBP_SIM_Fixed.docx'

# Extract both
os.makedirs('temp_template', exist_ok=True)
os.makedirs('temp_report', exist_ok=True)

with zipfile.ZipFile(template_path, 'r') as z:
    z.extractall('temp_template')

with zipfile.ZipFile(report_path, 'r') as z:
    z.extractall('temp_report')

# Copy styles and numbering
if os.path.exists('temp_template/word/styles.xml'):
    shutil.copy('temp_template/word/styles.xml', 'temp_report/word/styles.xml')
if os.path.exists('temp_template/word/numbering.xml'):
    shutil.copy('temp_template/word/numbering.xml', 'temp_report/word/numbering.xml')

# Copy theme just in case
if os.path.exists('temp_template/word/theme/theme1.xml'):
    shutil.copy('temp_template/word/theme/theme1.xml', 'temp_report/word/theme/theme1.xml')
if os.path.exists('temp_template/word/fontTable.xml'):
    shutil.copy('temp_template/word/fontTable.xml', 'temp_report/word/fontTable.xml')

# Re-zip report
def zipdir(path, ziph):
    for root, dirs, files in os.walk(path):
        for file in files:
            file_path = os.path.join(root, file)
            arcname = os.path.relpath(file_path, path)
            ziph.write(file_path, arcname)

with zipfile.ZipFile(output_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
    zipdir('temp_report', zipf)

print("Done generating Laporan_TBP_SIM_Fixed.docx")

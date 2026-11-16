import sys, os
from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas

INVOICES = [
    ("nimbus.pdf", "Nimbus Cloud Inc.", "billing@nimbuscloud.example", "INV-NC-20419", "2026-10-01", "2026-10-15",
     [("Compute, 2 vCPU instances, September", 36.40), ("Object storage, 118 GB", 11.80)]),
    ("streamtube.pdf", "StreamTube LLC", "receipts@streamtube.example", "ST-88123-1003", "2026-10-03", "2026-10-03",
     [("StreamTube Premium, monthly", 13.99)]),
    ("hostwerk.pdf", "Hostwerk Domains", "invoices@hostwerk.example", "HW-2026-5531", "2026-10-05", "2026-10-19",
     [("Domain renewal rivera.dev, 1 year", 18.00), ("DNS hosting, 1 year", 11.00)]),
    ("northline.pdf", "Northline Wireless", "bills@northline.example", "NW-771-1026", "2026-10-08", "2026-10-22",
     [("Business plan, 1 line, October", 55.00), ("Roaming add-on", 10.00)]),
    ("deskco.pdf", "Desk & Co Coworking", "accounts@deskco.example", "DC-1026-14", "2026-10-10", "2026-11-09",
     [("Dedicated desk, November", 240.00)]),
]

def render(path, vendor, email, number, date, due, lines):
    c = canvas.Canvas(path, pagesize=letter)
    w, h = letter
    c.setFont("Helvetica-Bold", 20); c.drawString(54, h - 72, vendor)
    c.setFont("Helvetica", 10); c.drawString(54, h - 90, email)
    c.setFont("Helvetica-Bold", 14); c.drawString(400, h - 72, "INVOICE")
    c.setFont("Helvetica", 10)
    c.drawString(400, h - 90, "Invoice no.  " + number)
    c.drawString(400, h - 104, "Date         " + date)
    c.drawString(400, h - 118, "Due          " + due)
    c.drawString(54, h - 150, "Bill to: Alex Rivera, Rivera Software, 12 Harbor St, Portland, OR")
    y = h - 200
    c.setFont("Helvetica-Bold", 10); c.drawString(54, y, "Description"); c.drawRightString(w - 54, y, "Amount (USD)")
    c.setFont("Helvetica", 10)
    for d, a in lines:
        y -= 18; c.drawString(54, y, d); c.drawRightString(w - 54, y, "%.2f" % a)
    y -= 28
    c.setFont("Helvetica-Bold", 12)
    c.drawString(54, y, "Total due"); c.drawRightString(w - 54, y, "$%.2f" % sum(a for _, a in lines))
    c.setFont("Helvetica", 9)
    c.drawString(54, 60, "Please quote %s as payment reference." % number)
    c.save()

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
for f, *rest in INVOICES:
    render(os.path.join(out, f), *rest)
print("rendered", len(INVOICES))

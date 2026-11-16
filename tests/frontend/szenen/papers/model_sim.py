import json, re, sys, time
from http.server import BaseHTTPRequestHandler, HTTPServer


def text_of(messages):
    out = []
    for m in messages:
        c = m.get("content")
        if isinstance(c, str):
            out.append(c)
        elif isinstance(c, list):
            out += [p.get("text", "") for p in c if p.get("type") == "text"]
    return "\n".join(out)


def read_invoice(t):
    def grab(rx):
        m = re.search(rx, t)
        return m.group(1).strip() if m else None
    vendor = grab(r"text layer\]\n(.+?)\s+INVOICE")
    total = grab(r"Total due\s+\$?([\d,]+\.\d\d)")
    return {"vendor": vendor,
            "invoice-number": grab(r"Invoice no\.\s+(\S+)"),
            "invoice-date": grab(r"Date\s+(\d{4}-\d{2}-\d{2})"),
            "due-date": grab(r"Due\s+(\d{4}-\d{2}-\d{2})"),
            "total": float(total.replace(",", "")) if total else None}


class H(BaseHTTPRequestHandler):
    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        if len(sys.argv) > 2:
            with open(sys.argv[2], "w") as f:
                json.dump(body, f, indent=1)
        value = read_invoice(text_of(body.get("messages", [])))
        time.sleep(1.2)
        answer = {"id": "sim-%d" % int(time.time() * 1000), "object": "chat.completion",
                  "model": body.get("model"),
                  "choices": [{"index": 0, "finish_reason": "tool_calls",
                               "message": {"role": "assistant", "content": None,
                                           "tool_calls": [{"id": "call-1", "type": "function",
                                                           "function": {"name": "final_answer",
                                                                        "arguments": json.dumps({"value": value})}}]}}],
                  "usage": {"prompt_tokens": 900, "completion_tokens": 60, "total_tokens": 960}}
        out = json.dumps(answer).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(out)))
        self.end_headers()
        self.wfile.write(out)
        print("read", value, flush=True)

    def log_message(self, *a):
        pass


port = int(sys.argv[1]) if len(sys.argv) > 1 else 8765
print("model simulation on http://127.0.0.1:%d/v1" % port, flush=True)
HTTPServer(("127.0.0.1", port), H).serve_forever()

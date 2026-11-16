(import '(com.icegreen.greenmail.util GreenMail ServerSetup)
        '(jakarta.mail Message$RecipientType Session)
        '(jakarta.mail.internet InternetAddress MimeBodyPart MimeMessage MimeMultipart)
        '(java.util Properties Date)
        '(java.time LocalDate ZoneId))

(def mails
  [["nimbus.pdf"     "Nimbus Cloud <billing@nimbuscloud.example>"   "Your Nimbus Cloud invoice INV-NC-20419" "2026-10-01"]
   ["streamtube.pdf" "StreamTube <receipts@streamtube.example>"     "Your StreamTube Premium receipt"       "2026-10-03"]
   ["hostwerk.pdf"   "Hostwerk Domains <invoices@hostwerk.example>" "Invoice HW-2026-5531 for rivera.dev"  "2026-10-05"]
   ["northline.pdf"  "Northline Wireless <bills@northline.example>" "Your October bill is ready"           "2026-10-08"]
   ["deskco.pdf"     "Desk & Co <accounts@deskco.example>"          "Desk & Co invoice for November"       "2026-10-10"]])

(def dir (first *command-line-args*))
(def gm (doto (GreenMail. (into-array ServerSetup [(ServerSetup. 3143 "127.0.0.1" "imap")]))
          (.start)))
(def user (.setUser gm "alex@rivera.example" "alex" "film"))
(def session (Session/getInstance (Properties.)))

(doseq [[file from subject day] mails]
  (let [msg  (MimeMessage. session)
        text (doto (MimeBodyPart.) (.setText (str "Hello Alex,\n\nplease find your invoice attached.\n")))
        pdf  (doto (MimeBodyPart.) (.attachFile (java.io.File. dir file)) (.setHeader "Content-Type" "application/pdf"))]
    (.setFrom msg (InternetAddress. from))
    (.setRecipient msg Message$RecipientType/TO (InternetAddress. "alex@rivera.example"))
    (.setSubject msg subject)
    (.setSentDate msg (Date/from (.toInstant (.atStartOfDay (LocalDate/parse day) (ZoneId/of "America/Los_Angeles")))))
    (.setContent msg (doto (MimeMultipart.) (.addBodyPart text) (.addBodyPart pdf)))
    (.saveChanges msg)
    (.deliver user msg)))

(println "mailbox ready: imap://alex:film@127.0.0.1:3143 with" (count mails) "mails")
(flush)
@(promise)

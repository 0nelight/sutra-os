(ns sutra.tooling.git-hooks
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def marker "# sutra pre-push (generated)")

(defn absolutize
  [dir p]
  (when-not (str/blank? (str p))
    (let [f (io/file (str p))]
      (.getAbsolutePath (if (.isAbsolute f) f (io/file (or dir ".") (str p)))))))

(defn body
  [change-bin]
  (str "#!/bin/sh\n" marker "\n"
       "CHANGE=\"${SUTRA_CHANGE_BIN:-" change-bin "}\"\n"
       "[ -x \"$CHANGE\" ] || CHANGE=$(command -v sutra-change 2>/dev/null)\n"
       "[ -n \"$SUTRA_SKIP_PRECHECK\" ] && exit 0\n"
       "[ -x \"$CHANGE\" ] || exit 0\n"
       "ZU_PRUEFEN=0\n"
       "while read -r LREF LSHA RREF RSHA; do\n"
       "  [ -z \"$LREF\" ] && continue\n"
       "  case \"$LSHA\" in *[!0]*) ;; *) continue ;; esac\n"
       "  case \"$RREF\" in refs/heads/sketch/*) continue ;; esac\n"
       "  ZU_PRUEFEN=1\n"
       "done\n"
       "[ \"$ZU_PRUEFEN\" = 0 ] && exit 0\n"
       "ROOT=$(git rev-parse --show-toplevel) || exit 0\n"
       "OUT=$(\"$CHANGE\" status --offline --dir \"$ROOT\" 2>&1)\n"
       "if [ $? -eq 1 ]; then\n"
       "  printf '%s\\n' \"$OUT\" >&2\n"
       "  printf '\\n  push abgebrochen — dieselben Prüfungen laufen serverseitig im push.\\n' "
       ">&2\n"
       "  printf '  beheben, oder: git push --no-verify (bzw. SUTRA_SKIP_PRECHECK=1)\\n' >&2\n"
       "  exit 1\n"
       "fi\n"
       "exit 0\n"))

(def commit-marker "# sutra pre-commit (generated)")

(defn commit-body
  [trunk]
  (str "#!/bin/sh\n" commit-marker "\n"
       "[ -n \"$SUTRA_SKIP_PRECHECK\" ] && exit 0\n"
       "for s in rebase-merge rebase-apply MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD; do\n"
       "  [ -e \"$(git rev-parse --git-path $s)\" ] && exit 0\n"
       "done\n"
       "B=$(git symbolic-ref --quiet --short HEAD) || B=\n"
       "case \"$B\" in\n"
       "  \"\"|" trunk ")\n"
       "    printf '\\n  commit abgelehnt — %s nimmt keine Commits an (geschützt).\\n' "
       "\"${B:-HEAD}\" >&2\n"
       "    printf '  ein Commit hier kann nie gepusht werden. Erst: sutra change start "
       "\"<titel>\"\\n' >&2\n"
       "    printf '  (die Änderungen im Arbeitsbaum wandern mit)\\n' >&2\n"
       "    exit 1 ;;\n"
       "esac\n"
       "exit 0\n"))

(defn- install-one!
  [hooks-dir datei mark neu]
  (let [f   (io/file (str hooks-dir) datei)
        da? (.exists f)
        alt (when da? (slurp f))]
    (cond
      (and da? (not (str/includes? alt mark))) :fremd
      (= alt neu) :unverändert
      :else (do (io/make-parents f)
                (spit f neu)
                (.setExecutable f true)
                (if da? :aktualisiert :written)))))

(defn- remove-own!
  [hooks-dir datei mark]
  (let [f (io/file (str hooks-dir) datei)]
    (when (and (.exists f) (str/includes? (slurp f) mark))
      (io/delete-file f true))))

(defn install!
  ([hooks-dir change-bin] (install! hooks-dir change-bin nil))
  ([hooks-dir change-bin trunk-geschuetzt?]
   (if (str/blank? (str hooks-dir))
     :kein-ort
     (do (if (false? trunk-geschuetzt?)
           (remove-own! hooks-dir "pre-commit" commit-marker)
           (install-one! hooks-dir "pre-commit" commit-marker (commit-body "main")))
         (install-one! hooks-dir "pre-push" marker (body change-bin))))))

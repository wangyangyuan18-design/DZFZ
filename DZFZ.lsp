;;; DZFZ / DZXG - ZWCAD AutoLISP
;;; Supports TEXT and MTEXT. Increments trailing ASCII digits only.
;;; Version: 1.0.0

(vl-load-com)

(defun dz:digit-p (c)
  (and c (<= 48 c) (<= c 57))
)

(defun dz:split-suffix (s / i n)
  (setq n (strlen s)
        i n)
  (while (and (> i 0) (dz:digit-p (ascii (substr s i 1))))
    (setq i (1- i)))
  (if (= i n)
    nil
    (list (substr s 1 i) (substr s (1+ i)))))

(defun dz:format-number (num width / s)
  (setq s (itoa num))
  (if (< (strlen s) width)
    (strcat (substr "00000000000000000000" 1 (- width (strlen s))) s)
    s))

(defun dz:increment-text (s amount / parts prefix digits width value)
  (setq parts (dz:split-suffix s))
  (if parts
    (progn
      (setq prefix (car parts)
            digits (cadr parts)
            width (strlen digits)
            value (+ (atoi digits) amount))
      (if (< value 0)
        nil
        (strcat prefix (dz:format-number value width))))
    nil))

(defun dz:replace-suffix-number (s value minwidth / parts prefix digits width)
  (setq parts (dz:split-suffix s))
  (if parts
    (progn
      (setq prefix (car parts)
            digits (cadr parts)
            width (max (strlen digits) minwidth))
      (if (< value 0)
        nil
        (strcat prefix (dz:format-number value width))))
    nil))

(defun dz:get-text (ename / obj val ed typ chunks pair)
(setq ed (entget ename) typ (cdr (assoc 0 ed)))
(if (member typ '("TEXT" "MTEXT"))
(progn (setq obj (vlax-ename->vla-object ename))
(setq val (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
(if (vl-catch-all-error-p val)
(if (= typ "MTEXT") (progn (setq chunks "") (foreach pair ed (if (member (car pair) '(3 1)) (setq chunks (strcat chunks (cdr pair))))) chunks) (cdr (assoc 1 ed))) val)) nil))
(defun dz:set-text (ename newtext / obj result)
(setq obj (vlax-ename->vla-object ename))
(setq result (vl-catch-all-apply 'vlax-put-property (list obj 'TextString newtext)))
(not (vl-catch-all-error-p result)))
(defun dz:copy-text-entity (ename basept targetpt newtext / obj cp p1 p2 result)
(setq obj (vlax-ename->vla-object ename) p1 (trans basept 1 0) p2 (trans targetpt 1 0))
(setq result (vl-catch-all-apply 'vla-copy (list obj)))
(if (vl-catch-all-error-p result) nil
(progn (setq cp result)
(setq result (vl-catch-all-apply 'vla-move (list cp (vlax-3d-point p1) (vlax-3d-point p2))))
(if (vl-catch-all-error-p result) (progn (vl-catch-all-apply 'vla-delete (list cp)) nil)
(progn (setq result (vl-catch-all-apply 'vlax-put-property (list cp 'TextString newtext)))
(if (vl-catch-all-error-p result) (progn (vl-catch-all-apply 'vla-delete (list cp)) nil) T))))))


(defun dz:valid-entity-p (ename / ed typ)
  (and ename
       (setq ed (entget ename))
       (setq typ (cdr (assoc 0 ed)))
       (member typ '("TEXT" "MTEXT"))))

(defun dz:report-invalid (ename / ed typ)
(if (and ename (setq ed (entget ename))) (setq typ (cdr (assoc 0 ed))) (setq typ "Unknown object"))
(prompt (strcat "\nUnsupported object: " typ ". Select TEXT or MTEXT.")))


(defun c:DZFZ (/ *error* olderr en txt parts base pt step idx newtxt)
(setq olderr *error*)
(defun *error* (msg) (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))) (prompt (strcat "\nDZFZ error: " msg))) (setq *error* olderr) (princ))
(prompt "\nDZFZ - Incremental text copy")
(setq en (car (entsel "\nSelect source TEXT/MTEXT: ")))
(cond
((not en) (prompt "\nNo object selected."))
((not (dz:valid-entity-p en)) (dz:report-invalid en))
(T (setq txt (dz:get-text en) parts (dz:split-suffix txt))
(if (not parts) (prompt "\nNo trailing digits found.")
(progn (setq base (getpoint "\nSpecify copy base point: "))
(if base (progn
(setq step (getint "\nIncrement step <1>: "))
(if (or (null step) (= step 0)) (setq step 1))
(setq idx 1)
(prompt "\nClick each destination to place a copy. Press ESC to finish.")
(while (setq pt (getpoint "\nSpecify next copy location: "))
(setq newtxt (dz:increment-text txt (* idx step)))
(if newtxt (if (dz:copy-text-entity en base pt newtxt) (setq idx (1+ idx)) (prompt "\nCopy failed; skipped.")) (prompt "\nInvalid increment; stopping.")))))))))
(setq *error* olderr)
(princ))


(defun c:DZXG (/ *error* olderr en txt seed seedwidth parts count newtxt stop step)
(setq olderr *error* seed nil seedwidth 0 count 0 stop nil step 1)
(defun *error* (msg) (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))) (prompt (strcat "\nDZXG error: " msg))) (setq *error* olderr) (princ))
(prompt "\nDZXG - Increment text in pick order")
(setq step (getint "\nIncrement step <1>: "))
(if (or (null step) (= step 0)) (setq step 1))
(prompt "\nPick TEXT/MTEXT in order. First valid object is seed. Press ESC to finish.")
(while (and (not stop) (setq en (car (entsel "\nSelect next text object: "))))
(if (dz:valid-entity-p en)
(progn (setq txt (dz:get-text en) parts (dz:split-suffix txt))
(if (= count 0)
(if parts (progn (setq seed (atoi (cadr parts)) seedwidth (strlen (cadr parts)) count 1) (prompt "\nSeed recorded."))
(progn (prompt "\nFirst object has no trailing digits. Stopped.") (setq stop T)))
(if parts
(progn (setq newtxt (dz:replace-suffix-number txt (+ seed (* count step)) seedwidth))
(if (and newtxt (dz:set-text en newtxt)) (progn (entupd en) (prompt (strcat "\nUpdated: " newtxt)) (setq count (1+ count))) (prompt "\nUpdate failed.")))
(prompt "\nNo trailing digits; skipped."))))
(dz:report-invalid en)))
(prompt (strcat "\nDZXG finished. Valid object count: " (itoa count)))
(setq *error* olderr)
(princ))



(prompt "\nDZFZ/DZXG loaded. Type DZFZ or DZXG to start.")
(princ)

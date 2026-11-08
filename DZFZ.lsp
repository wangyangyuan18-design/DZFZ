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

(defun dz:get-text (edata / typ item)
  (setq typ (cdr (assoc 0 edata)))
  (if (member typ '("TEXT" "MTEXT"))
    (cdr (assoc 1 edata))
    nil))

(defun dz:set-text-data (edata newtext / out done pair)
  (setq out '()
        done nil)
  (foreach pair edata
    (if (= (car pair) 1)
      (if (not done)
        (progn
          (setq out (cons (cons 1 newtext) out))
          (setq done T)))
      (setq out (cons pair out))))
  (if done (reverse out) nil))

(defun dz:move-data (edata dx dy dzv / out pair val)
  (setq out '())
  (foreach pair edata
    (if (member (car pair) '(10 11))
      (progn
        (setq val (cdr pair))
        (if (and (listp val) (>= (length val) 2))
          (setq out
            (cons
              (cons (car pair)
                (list (+ (car val) dx)
                      (+ (cadr val) dy)
                      (+ (if (caddr val) (caddr val) 0.0) dzv)))
              out))
          (setq out (cons pair out))))
      (setq out (cons pair out))))
  (reverse out))

(defun dz:valid-entity-p (ename / ed typ)
  (and ename
       (setq ed (entget ename))
       (setq typ (cdr (assoc 0 ed)))
       (member typ '("TEXT" "MTEXT"))))

(defun dz:report-invalid (ename / ed typ)
  (if (and ename (setq ed (entget ename)))
    (setq typ (cdr (assoc 0 ed)))
    (setq typ "未知对象"))
  (prompt (strcat "\n不支持的对象：" typ "。请选择 TEXT 或 MTEXT。")))

(defun c:DZFZ (/ *error* olderr en ed txt parts base pt step idx newtxt moved clone)
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (prompt (strcat "\nDZFZ 错误：" msg)))
    (setq *error* olderr)
    (princ))
  (prompt "\nDZFZ - 文字递增复制")
  (setq en (car (entsel "\n选择作为模板的 TEXT/MTEXT：")))
  (cond
    ((not en) (prompt "\n未选择对象，命令结束。"))
    ((not (dz:valid-entity-p en)) (dz:report-invalid en))
    (T
      (setq ed (entget en)
            txt (dz:get-text ed)
            parts (dz:split-suffix txt))
      (if (not parts)
        (prompt "\n模板文字末尾没有连续数字，无法递增。")
        (progn
          (setq base (getpoint "\n指定复制基点："))
          (if base
            (progn
              (setq step (getint "\n输入递增步长 <1>："))
              (if (or (null step) (= step 0)) (setq step 1))
              (setq idx 1)
              (prompt "\n逐次点击目标位置放置副本，按 ESC 结束。")
              (while (setq pt (getpoint "\n指定下一个副本位置："))
                (setq newtxt (dz:increment-text txt (* idx step)))
                (if newtxt
                  (progn
                    (setq moved (dz:move-data ed
                                      (- (car pt) (car base))
                                      (- (cadr pt) (cadr base))
                                      (- (if (caddr pt) (caddr pt) 0.0)
                                         (if (caddr base) (caddr base) 0.0))))
                    (setq clone (dz:set-text-data moved newtxt))
                    ;; Remove entity identity and ownership data before entmakex.
                    (setq clone
                      (vl-remove-if
                        '(lambda (x) (member (car x) '(-1 5 330 360)))
                        clone))
                    (if (entmakex clone)
                      (setq idx (1+ idx))
                      (prompt "\n副本创建失败，已跳过该位置。")))
                  (prompt "\n数字递增结果无效，已停止放置。")))))))
  (setq *error* olderr)
  (princ))

(defun c:DZXG (/ *error* olderr en ed txt seed parts count newtxt newed)
  (setq olderr *error*
        seed nil
        count 0)
  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (prompt (strcat "\nDZXG 错误：" msg)))
    (setq *error* olderr)
    (princ))
  (prompt "\nDZXG - 按点击顺序递增修改文字")
  (prompt "\n依次点击 TEXT/MTEXT；第一个对象作为递增起点，按 ESC 结束。")
  (while (setq en (car (entsel "\n选择下一个文字对象：")))
    (if (dz:valid-entity-p en)
      (progn
        (setq ed (entget en)
              txt (dz:get-text ed))
        (if (= count 0)
          (progn
            (setq parts (dz:split-suffix txt))
            (if parts
              (setq seed (atoi (cadr parts)))
              (prompt "\n第一个对象末尾没有连续数字；请重新启动 DZXG 并选择有效文字。")))
          (if seed
            (progn
              (setq newtxt (dz:increment-text txt count))
              (if newtxt
                (progn
                  (setq newed (dz:set-text-data ed newtxt))
                  (if (entmod newed)
                    (progn (entupd en) (prompt (strcat "\n已修改为：" newtxt)))
                    (prompt "\n修改失败。")))
                (prompt "\n该文字末尾没有连续数字，已跳过。"))))
        (if (or (= count 0) seed)
          (setq count (1+ count))))
      (dz:report-invalid en)))
  (prompt (strcat "\nDZXG 结束。本次处理对象数：" (itoa count)))
  (setq *error* olderr)
  (princ))

(prompt "\nDZFZ/DZXG 已加载。输入 DZFZ 或 DZXG 启动。")
(princ)

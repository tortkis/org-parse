
;;;; org-parse.lisp
;;;;
;;;; parse org file; convert to s-exp html; create hunchentoot dispatcher
;;;;

(in-package :cl-user)

(defpackage :org-parse
  (:use :cl)
  (:export
   :read-org-file
   :org-to-shtml
   :org-to-shtml-w-head
   :export-org-to-html
   :define-ht-org-dispatcher
   :define-ht-org))

(in-package :org-parse)

;;; parameters

(defvar *shtml-head*
  '(((:style :type "text/css")
     "<!--"
     "  pre {"
     "        background-color: #eeeeee;"
     "        padding: 5pt;"
     "        font-family: courier, monospace;"
     "        font-size: 90%;"
     "        overflow:auto;"
     "  }"
     "  h1, h2, h3, h4, h5, h6 { font-family: sans-serif; }"
     "-->")))

(defvar *shtml-table-attributes*
  '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"))

(defvar *org-color*
  '((:todo . "red")
    (:done . "green")
    (:other . "blue")
    (:date . "#999")
    (:checklist-done . "green")
    (:checklist-yet . "red")
    (:tag . "green")))

(defvar *image-types*
  '("png" "jpg" "jpeg" "gif"))

;;; parse org

(let ((line-buf (make-hash-table)))
  (defun read-line-buf (str)
    (cond ((gethash str line-buf)
           (pop (gethash str line-buf)))
          (t
           (read-line str nil nil))))
  (defun push-line-buf (str line)
    (push line (gethash str line-buf))))

(defun line-offset (line)
  (position-if #'(lambda (c) (not (find c '(#\Space #\Tab)))) line))

(defun sep-1st-word (line)
  (let* ((line1 (string-trim '(#\Space #\Tab) line))
         (p1 (cond ((and (> (length line1) 0) (find (char line1 0) '(#\[ #\<)))
                    (let ((pe (position (case (char line1 0) (#\[ #\]) (#\< #\>))
                                        line1 :start 1)))
                      (if pe (1+ pe))))
                   (t (position-if #'(lambda (c) (find c '(#\Space #\Tab))) line1))))
         (w1 (subseq line1 0 p1))
         (line-rest (if p1 (string-trim '(#\Space #\Tab) (subseq line1 p1)))))
    (list w1 line-rest)))

(defun checkbox-sum (w)
  (let* ((ps (position #\/ w))
         (n-done (if ps (parse-integer (subseq w 1 ps) :junk-allowed t)))
         (n-total (if ps (parse-integer (subseq w (1+ ps) (1- (length w))) :junk-allowed t))))
    (when (and n-done n-total)
      (list n-done n-total))))

(defun parse-headline (line)
  (let* ((line-sep (sep-1st-word line))
         (w1 (car line-sep))
         (todo-key (if (find w1 '("TODO" "DONE") :test #'string-equal) w1))
         (line2 (if todo-key (cadr line-sep) (string-trim '(#\Space #\Tab) line))))
    (labels ((sep-org-key (line)
               (let ((ws (sep-1st-word line)))
                 (cond ((or (null line) (< (length line) 1)) nil)
                       ((find (char (car ws) 0) '(#\[ #\<))
                        (cons (case (char (car ws) 0)
                                (#\[ (let ((cb-sum (checkbox-sum (car ws))))
                                       (if cb-sum
                                           (list :checklist cb-sum)
                                           (car ws))))
                                (#\< (list :date (car ws))))
                              (sep-org-key (cadr ws))))
                       ((and (null (cadr ws))
                             (eql (char (car ws) 0) #\:)
                             (eql (char (car ws) (1- (length (car ws)))) #\:))
                        (list (list :tag (car ws))))
                       (t (let ((hl-rest (sep-org-key (cadr ws))))
                            (cond ((stringp (car hl-rest))
                                   (cons (format nil "~A ~A" (car ws) (car hl-rest))
                                         (cdr hl-rest)))
                                  (t (cons (car ws) hl-rest)))))))))
      (cons todo-key (sep-org-key line2)))))

(defun get-headline-key (line)
  ;; returns list of headline type and depth
  (let* ((p0 (position-if-not #'(lambda (c) (find c '(#\Space #\Tab))) line))
         (p1 (if p0 (position-if #'(lambda (c) (find c '(#\Space #\Tab))) line :start p0)))
         (key (if p0 (subseq line p0 p1)))
         (p2 (if p1 (position-if-not #'(lambda (c) (find c '(#\Space #\Tab))) line :start p1)))
         (body (if p2 (subseq line p2))))
    (cond ((or (null key) (< (length key) 1)) nil)
          ((every #'(lambda (c) (eql c  #\*)) key)
           (list :* (1- (length key)) (parse-headline body)))
          ((equal key "-") (list :- p0 body))
          ((equal key "+") (list :+ p0 body))
          ((and (every #'(lambda (c) (digit-char-p c)) (subseq key 0 (1- (length key))))
                (find (char key (1- (length key))) '(#\) #\.)))
           (list :ol p0 body))
          (t nil))))

(defun ch1st (line)
  (find-if #'(lambda (c) (not (find c '(#\Space #\Tab)))) line))

(defun read-pre (str &optional (offset 0))
  (let* ((line1 (subseq (read-line-buf str) offset))
         (ch1 (ch1st line1)))
    (labels ((read-pre-rec (str)
               (let* ((line (read-line-buf str)))
                 (cond ((not (eql (ch1st line) ch1))
                        (if line (push-line-buf str line))
                        nil)
                       (t
                        (cons (subseq line offset)
                              (read-pre-rec str)))))))
      (cons line1 (read-pre-rec str)))))

(defun tbl-line-div (tbl-line)
  (let* ((p-col-left (position #\| tbl-line))
         (p-col-right (if p-col-left
                          (position #\| tbl-line :start (1+ p-col-left))))
         (p0 (if p-col-right
                 (position-if-not #'(lambda (c) (find c '(#\Space #\Tab)))
                                  tbl-line :start (1+ p-col-left))))
         (p1 (if p-col-right
                 (position-if-not #'(lambda (c) (find c '(#\Space #\Tab)))
                                  (subseq tbl-line 0 p-col-right) :from-end t)))
         (item (if (and p0 p1 (>= p1 p0)) (subseq tbl-line p0 (1+ p1)) "")))
    (cond ((null p-col-right) nil)
          ((eql (char tbl-line (1+ p-col-left)) #\-)
           (list "-"))
          (t (cons item (tbl-line-div (subseq tbl-line p-col-right)))))))

(defun read-tbl (str)
  (mapcar #'tbl-line-div (read-pre str)))

(defun div-tbl (tbl)
  (let ((psep (position-if #'(lambda (row)
                               (and (> (length (car row)) 0)
                                    (eql (char (car row) 0) #\-)))
                           tbl)))
    (cond ((null psep) (list tbl))
          ((= psep 0) (div-tbl (subseq tbl 1)))
          (t (cons (subseq tbl 0 psep)
                   (div-tbl (subseq tbl (1+ psep))))))))

(defun line-begin-with (line s)
  ;; returns rest of the string if s is found in line
  (let* ((p0 (position-if-not #'(lambda (c) (find c '(#\Space #\Tab))) line))
         (p1 (if p0 (search s (subseq line p0) :test #'string-equal))))
    (when (and p1 (= p1 0))
      (subseq line (+ p0 (length s))))))

(defun read-upto (str pred-or-s &optional exclude-last)
  (let ((line (read-line-buf str)))
    (cond ((null line) nil)
          ((or (and (stringp pred-or-s)
                    (line-begin-with line pred-or-s))
               (and (functionp pred-or-s)
                    (funcall pred-or-s line)))
           (if exclude-last
               nil
               (list line)))
          (t (cons line (read-upto str pred-or-s exclude-last))))))

(defun read-before (str pred-or-s)
  (read-upto str pred-or-s t))

(defun string-to-sexp (s)
  (labels ((R (s i)
             (let ((p1 (position-if-not
                        #'(lambda (x) (or (eql x #\Space) (eql x #\Tab)))
                        s :start i)))
               (when (and p1 (<= p1 (- (length s) 1)))
                 (let* ((p2 (position-if
                             #'(lambda (x) (or (eql x #\Space) (eql x #\Tab)))
                             s :start p1)))
                   (if (not p2)
                       (list (subseq s p1))
                       (cons (subseq s p1 p2) (R s p2))))))))
    (R s 0)))

(defun read-block (str)
  (let* ((line1 (read-line-buf str))
         (line-rest (string-to-sexp (line-begin-with line1 "#+BEGIN_"))))
    (cond ((not line-rest)
           (push-line-buf str line1)
           nil)
          (t (cons (cons :block line-rest)
                   (read-before str (format nil "#+END_~A" (car line-rest))))))))

(defun line-type (line hl-key)
  (let ((ch1 (ch1st line))
        (hl-key-new (if line (get-headline-key line))))
    (cond ((null line) nil)
          ((or (and hl-key hl-key-new
                    (eql (car hl-key) :*)
                    (eql (car hl-key-new) :*)
                    (<= (cadr hl-key-new) (cadr hl-key)))
               (and hl-key
                    (find (car hl-key) '(:- :+ :ol))
                    (line-offset line)
                    (<= (line-offset line) (cadr hl-key)))
               (and hl-key hl-key-new
                    (find (car hl-key) '(:- :+ :ol))
                    (or (eql (car hl-key-new) :*)
                        (and (eql (car hl-key-new) (car hl-key))
                             (<= (cadr hl-key-new) (cadr hl-key)))))
               (null hl-key))
           :end-outline-block)
          (hl-key-new :start-outline-block)
          ((line-begin-with line "#+BEGIN_") :block)
          ((eql ch1 #\|) :table)
          ((find ch1 '(#\# #\: #\>)) :literal)
          (t :other))))

(defun link-cnv (link dir)
  (if (eql (char link 0) #\/)
      link
      (let* ((pc (search "./" link))
             (link2 (if (and pc (= pc 0)) (subseq link 2) link)))
        (concatenate 'string dir link2))))

(defun org-line-parse (line dir)
  (let* ((pl0 (search "[[" line))
         (pl1 (if pl0 (search "]]" line :start2 pl0))))
    (cond (pl1
           (remove
            nil
            (cons (when (> pl0 0)
                    (subseq line 0 pl0))
                  (cons
                   (let* ((link-line (subseq line (1+ pl0) (1+ pl1)))
                          (ps (search "][" link-line))
                          (link (if ps
                                    (subseq link-line 1 ps)
                                    (subseq link-line 1 (1- (length link-line))))))
                     (list :link
                           (link-cnv link dir)
                           (if ps
                               (subseq link-line (+ ps 2) (1- (length link-line)))
                               link)))
                   (when (< pl1 (- (length line) 2))
                     (org-line-parse (subseq line (+ pl1 2)) dir))))))
          (t (list line)))))
           

(defun read-org-1 (str dir)
  (let* ((line1 (read-line-buf str))
         (hl-key (if line1 (get-headline-key line1)))
         (head-offset (if (line-offset line1)
                          (+ (line-offset line1)
                             (if (and hl-key (not (eql (car hl-key) :*))) 1 0)))))
    (labels ((read-org-rec ()
               (let* ((line (read-line-buf str)))
                 (case (line-type line hl-key)
                   ((nil) nil)
                   (:table
                    (push-line-buf str line)
                    (cons (cons (list :table (- (line-offset line) head-offset))
                                (read-tbl str))
                          (read-org-rec)))
                   (:literal
                    (push-line-buf str line)
                    (let ((pre-lines (read-pre str head-offset)))
                      (cond ((find (ch1st line) '(#\: #\>))
                             (cons (cons (list :pre (- (line-offset line) head-offset))
                                         pre-lines) (read-org-rec)))
                            (t (read-org-rec)))))
                   (:block
                    (push-line-buf str line)
                    (let ((block-lines (read-block str)))
                      (cons
                       (cons '(:pre 0) (cons (format nil ":#BLOCK~{ ~A~}" (cdar block-lines))
                                             (mapcar #'(lambda (x) (format nil ":~A" x))
                                                     (cdr block-lines))))
                       (read-org-rec))))
                   (:end-outline-block
                    (push-line-buf str line)
                    nil)
                   (:start-outline-block
                    (push-line-buf str line)
                    (cons (read-org-1 str dir) (read-org-rec)))
                   (otherwise
                    (cons (cons :line
                                (org-line-parse
                                 (if (line-offset line)
                                     (subseq line head-offset)
                                     line)
                                 dir))
                          (read-org-rec)))))))
      (cond (hl-key
             (cons hl-key (read-org-rec)))
            (t
             line1)))))

(defun org-make-list (org)
  (cond ((null org) nil)
        ((and (listp (car org))
              (listp (caar org))
              (find (caaar org) '(:- :+ :ol)))
         (let* ((list-type (caaar org))
                (level (cadaar org))
                (p1 (position-if #'(lambda (item)
                                     (not (and (listp item)
                                               (listp (car item))
                                               (eql (caar item) list-type)
                                               (= (cadar item) level))))
                                 org)))
           (cons (cons (if (eql list-type :ol) :list-o :list-u)
                       (mapcar #'(lambda (x) (org-make-list x)) (subseq org 0 p1)))
                 (if p1 (org-make-list (subseq org p1))))))
        ((listp (car org))
         (cons (org-make-list (car org))
               (org-make-list (cdr org))))
        (t (cons (car org) (org-make-list (cdr org))))))

(defun read-org (str dir)
  (labels ((read-org-rec ()
             (let ((org (read-org-1 str dir)))
               (cond ((null org) nil)
                     (t (cons org (read-org-rec)))))))
    (org-make-list (read-org-rec))))

(defun read-org-file (fn)
  (with-open-file (str fn)
    (read-org str (namestring (make-pathname :directory (pathname-directory fn))))))

;;; converting list to s-html

(defun shtml-table (tbl)
  (let ((indent (cadar tbl))
        (tbl-lis (cdr tbl)))
    (cons
     (append `(:table ,@*shtml-table-attributes*)
             (if (> indent 0)
                 `(:style ,(format nil "margin-left:~,1Fem;" (/ indent 2)))))
     (mapcar #'(lambda (tbl-sec)
                 `(:tbody
                   ,@(remove
                      nil
                      (mapcar #'(lambda (row)
                                  (unless (and (> (length (car row)) 0)
                                               (eql (char (car row) 0) #\-))
                                    `(:tr ,@(mapcar #'(lambda (item) `(:td ,item)) row))))
                              tbl-sec))))
             (div-tbl tbl-lis)))))

(defun shtml-pre (pre)
  (let ((indent (cadar pre))
        (pre-lis (cdr pre)))
    (cons (if (> indent 0)
              `(:pre :style ,(format nil "margin-left:~,1Fem;" (/ indent 2)))
              :pre)
          (mapcar #'(lambda (line)
                      (format nil "~A~%" (string-left-trim '(#\Space #\Tab) line)))
                  pre-lis))))

(defun shtml-add-indent (line)
  (let ((p1 (line-offset line)))
    (if (and p1 (> p1 0))
        `(((:div :style ,(format nil "margin-left:~,1Fem;" (/ p1 2))) ,line))
        (list line :br))))

(defun shtml-headline (hl)
  `(:div
    ,(if (car hl)
         (list :b (list `(:font
                          :color
                          ,(cond ((equal (car hl) "TODO") (cdr (assoc :todo *org-color*)))
                                 ((equal (car hl) "DONE") (cdr (assoc :done *org-color*)))
                                 (t (cdr (assoc :other *org-color*)))))
                        (format nil " ~A " (car hl))))
         "")
    ,@(mapcar
       #'(lambda (item)
           (cond ((stringp item) item)
                 ((listp item)
                  (let ((w (format nil "~{ ~A~} " (cdr item))))
                    (case (car item)
                      (:date `((:font :color ,(cdr (assoc :date *org-color*))) ,w))
                      (:checklist `((:font
                                     :color ,(if (apply #'< (cadr item))
                                                 (cdr (assoc :checklist-yet *org-color*))
                                                 (cdr (assoc :checklist-done *org-color*))))
                                    ,(format nil " [~A/~A]"
                                             (car (cadr item)) (cadr (cadr item)))))
                      (:tag `(:b ((:font :color ,(cdr (assoc :tag *org-color*))) ,w))))))))
              (cdr hl))))

(defun shtml-checkbox-line (line)
  (let ((ws (sep-1st-word line)))
    (cond ((string-equal (car ws) "[X]")
           (cons '(:input :type "checkbox" :disabled "disabled" :checked "checked")
                 (cdr ws)))
          ((string-equal (car ws) "[ ]")
           (cons '(:input :type "checkbox" :disabled "disabled")
                 (cdr ws)))
          (t line))))

(defun org-to-shtml (org &key inline-image)
  (labels ((org-to-shtml-rec (org no-indent)
             (let ((item (car org)))
               (cond ((null item) nil)
                     ((stringp item)
                      (append (if no-indent
                                  (list item)
                                  (shtml-add-indent item))
                              (org-to-shtml-rec (cdr org) no-indent)))
                     ((listp item)
                      (let ((shtml1
                             (cond ((listp (car item))
                                    (cond ((symbolp (caar item))
                                           (cond ((eql (caar item) :*)
                                                  `(:div (,(intern (format nil "h~A"
                                                                           (+ (cadr (car item)) 2)))
                                                           ,(shtml-headline (caddr (car item))))
                                                         ,@(org-to-shtml-rec (cdr item) no-indent)))
                                                 ((find (caar item) '(:- :+ :ol))
                                                  `(:li ,(shtml-checkbox-line (caddr (car item))) :br
                                                        ,@(org-to-shtml-rec (cdr item) no-indent)))
                                                 ((eql (caar item) :table)
                                                  (shtml-table item))
                                                 ((eql (caar item) :pre)
                                                  (shtml-pre item))))
                                          (t "FORMAT ERROR")))
                                   ((symbolp (car item))
                                    (cond ((eql (car item) :list-u)
                                           `(:ul ,@(org-to-shtml-rec (cdr item) no-indent)))
                                          ((eql (car item) :list-o)
                                           `(:ol ,@(org-to-shtml-rec (cdr item) no-indent)))
                                          ((eql (car item) :link)
                                           ;;`((:a :href ,(cadr item)) ,(or (caddr item) "")))
                                           (if (and inline-image
                                                    (find (pathname-type (cadr item))
                                                          *image-types* :test #'string-equal))
                                               `((:img :src ,(cadr item) :alt ,(or (caddr item) "")))
                                               `((:a :href ,(cadr item)) ,(or (caddr item) ""))))
                                          ((eql (car item) :line)
                                           (cons :div
                                                 (remove :br (org-to-shtml-rec (cdr item) t))))
                                          )))))
                        (cons shtml1 (org-to-shtml-rec (cdr org) no-indent))))))))
    (org-to-shtml-rec org nil)))

(defun org-to-shtml-w-head (org title &key inline-image)
  `(:html
    (:head
     (:title ,(or title (car org)))
     ,@*shtml-head*)
    (:body
     ,@(org-to-shtml org :inline-image inline-image))))

(defun export-org-to-html (org-fn html-fn &key inline-image)
  (with-open-file (str html-fn :direction :output :if-exists :supersede)
    (princ (html-string:html-string
            (org-to-shtml-w-head (read-org-file org-fn) (pathname-name org-fn)
                                 :inline-image inline-image))
           str))
  nil)

;;; define Hunchentoot dispatcher from org shtml 

(defun uri-cnv (uri)
  (let ((len (length uri)))
    (when (and (stringp uri) (> len 0))
      (format nil "~A~A"
              (if (eql (char uri 0) #\/) "" "/")
              (if (eql (char uri (1- len)) #\/) (subseq uri 0 (1- len)) uri)))))

(defun common-path (files)
  (let ((dir1st (mapcar #'(lambda (f)
                            (let ((p (position #\/ f)))
                              (if p (subseq f 0 (1+ p)) "")))
                        files)))
    (cond ((null files) nil)
          ((= (length files) 1)
           (directory-namestring (car files)))
          ((and (every #'(lambda (d) (equal d (car dir1st))) (cdr dir1st))
                (not (equal (car dir1st) "")))
           (format nil "~A~A"
                   (car dir1st)
                   (common-path (mapcar #'(lambda (f)
                                            (subseq f (length (car dir1st))))
                                        files))))
          (t ""))))

(defun remove-common-path (files)
  (let ((len (length (common-path files))))
    (mapcar #'(lambda (f) (subseq f len)) files)))

(defun abs-path (path &optional default-path)
  (namestring
   (merge-pathnames (pathname path) (or default-path *default-pathname-defaults*))))

(defun make-file-uri-alist (uri files &optional default-path)
  (mapcar #'list
          files
          (mapcar #'(lambda (f) (abs-path f default-path)) files)
          (mapcar #'(lambda (f) (format nil "~A/~A" (uri-cnv uri) f))
                  (remove-common-path files))))

(defun shtml-inline-image (inline-image uri shtml &optional default-path)
  ;; returns shtml in which the image files are converted to
  ;; inline (if inline-image is true) or link (if inline-image is nil)
  ;; and a list of image files
  (let ((image-files nil))
    (labels ((conv-ref (shtml &optional img-alist)
               (cond ((null shtml) nil)
                     ((and (listp (car shtml))
                           (listp (caar shtml))
                           (or (and (eql (first (caar shtml)) :a)
                                    (eql (second (caar shtml)) :href))
                               (and (eql (first (caar shtml)) :img)
                                    (eql (second (caar shtml)) :src)))
                           (find (pathname-type (third (caar shtml)))
                                 *image-types* :test #'string-equal))
                      (push (third (caar shtml)) image-files)
                      (cons (let ((link (append (if inline-image '(:img :src) '(:a :href))
                                                (if img-alist
                                                    (list (caddr (assoc (caddr (caar shtml)) img-alist
                                                                        :test #'equal)))
                                                    (cddr (caar shtml)))))
                                  (alt (if (eql (first (caar shtml)) :img)
                                           (fifth (caar shtml))
                                           (second (car shtml)))))
                              (if inline-image
                                  (list (append link (list :alt alt)))
                                  (cons link (list alt))))
                            (conv-ref (cdr shtml) img-alist)))
                     ((listp (car shtml))
                      (cons (conv-ref (car shtml) img-alist) (conv-ref (cdr shtml) img-alist)))
                     (t (cons (car shtml) (conv-ref (cdr shtml) img-alist))))))
      (conv-ref shtml) ; make image-files
      (let ((img-alist (make-file-uri-alist uri image-files default-path)))
        (values (conv-ref shtml img-alist) img-alist)))))

(defmacro define-ht-org-dispatcher (uri org-shtml
                                    &key (default-expand t) (return-uri nil) (inline-image nil)
                                    (default-path nil))
  `(let* ((head (find-if #'(lambda (blk) (and (listp blk)
                                              (eql (car blk) :head)))
                         ,org-shtml))
          (title (cadr (find-if #'(lambda (x) (and (listp x) (eql (car x) :title))) head)))
          (body (cdr (find-if #'(lambda (blk) (and (listp blk)
                                                   (eql (car blk) :body)))
                              ,org-shtml)))
          (shtml-n-images (multiple-value-list
                           (shtml-inline-image ,inline-image ,uri body ,default-path)))
          (images (cadr shtml-n-images))
          (idnum 0)
          (id-lis nil)
          (shtml
           (mapcar #'(lambda (blk)
                       (if (and (listp blk) (eql (car blk) :div) (eql (caadr blk) (intern "h2")))
                           (let ((id (format nil "i_~A_~A" ,(uri-cnv uri) (incf idnum))))
                             (push id id-lis)
                             (cons :div
                                   (cons
                                    (cons `(:h2 :id ,id)
                                          (cdr (cadr blk)))
                                    (cddr blk))))
                           blk))
                   (car shtml-n-images))))
     (push (hunchentoot:create-prefix-dispatcher
            ,(uri-cnv uri)
            ',(intern (string-upcase (format nil "page-~A" (uri-cnv uri)))))
           hunchentoot:*dispatch-table*)
     (mapc #'(lambda (img)
               (push (hunchentoot:create-static-file-dispatcher-and-handler
                      (caddr img) (cadr img))
                     hunchentoot:*dispatch-table*))
           images)
     (defun ,(intern (string-upcase (format nil "page-~A" (uri-cnv uri)))) ()
       (hunchentoot:start-session)
       (setq hunchentoot:*hunchentoot-default-external-format*
             (flex:make-external-format :utf-8 :eol-style :lf))
       (let ((hl (hunchentoot:post-parameter "hl")))
         (when (and hl (stringp hl))
           (multiple-value-bind (hl-exp exists)
               (hunchentoot:session-value (intern hl))
             (setf (hunchentoot:session-value (intern hl))
                   (if exists (not hl-exp) (not ,default-expand))))))
       ,@(if return-uri
             `((when (hunchentoot:post-parameter "return")
                 (hunchentoot:redirect ,return-uri))))
       (when (hunchentoot:post-parameter "expand")
         (mapc #'(lambda (id) (setf (hunchentoot:session-value (intern id)) t)) id-lis))
       (when (hunchentoot:post-parameter "collapse")
         (mapc #'(lambda (id) (setf (hunchentoot:session-value (intern id)) nil)) id-lis))
       (let* ((show-image (cond ((equal (hunchentoot:post-parameter "inline_image") "on")
                                 t)
                                ((equal (hunchentoot:post-parameter "inline_image") "off")
                                 nil)
                                (t ,inline-image)))
              (shtml-img (shtml-inline-image show-image ,uri shtml ,default-path))
              (shtml2
               `((:form :name "Form1" :id "Form1"
                        :action ,,(uri-cnv uri)
                        :method "post"
                        :onsubmit "saveScroll()")
                 ((:input :name "ScrollX" :id "ScrollX" :type "hidden" :value ""))
                 ((:input :name "ScrollY" :id "ScrollY" :type "hidden" :value ""))
                 ,@(if ,return-uri '(((:button :type "submit" :name "return") "return") "&nbsp;"))
                 ((:button :type "submit" :name "expand") "expand all")
                 ((:button :type "submit" :name "collapse") "collapse all")
                 ,@(when images
                     `(" inline image "
                       ((:input :type "radio" :name "inline_image" :value "on"
                                ,@(if show-image '(:checked "checked") nil))) " on "
                       ((:input :type "radio" :name "inline_image" :value "off"
                                ,@(if show-image nil '(:checked "checked")))) " off "
                       ((:button :type "submit" :name "submit") "reload")))
                 :br
                 (:h1 ,title)
                 ,@(mapcar
                    #'(lambda (blk)
                        (let* ((id (if (listp blk) (third (caadr blk))))
                               (exp-p (when id
                                        (multiple-value-bind (hl-exp exists)
                                            (hunchentoot:session-value (intern id))
                                          (if exists
                                              hl-exp
                                              (setf (hunchentoot:session-value (intern id))
                                                    ,default-expand))))))
                          (cond ((and (listp blk)
                                      (eql (car blk) :div)
                                      (eql (caaadr blk) :h2))
                                 `(:div
                                   ,(list (caadr blk)
                                          (cons
                                           :div
                                           (cons 
                                            `((:button
                                               :style "display:inline-block; vertical-align:middle"
                                               ,@(if (cddr blk) nil '(:disabled "disabled"))
                                               :type "submit"
                                               :name "hl"
                                               :value ,id)
                                              (:code
                                               ,(if (cddr blk)
                                                    (if exp-p "v" "&gt;")
                                                    "&nbsp;")))
                                            (cons "&nbsp;"
                                                  (cdr (second (cadr blk)))))))
                                   ,@(if exp-p (cddr blk) '())))
                                (t blk))))
                    shtml-img)
                 ((:script :language "javascript")
                  ,(format nil "window.scrollBy(~A,~A);"
                           (hunchentoot:post-parameter "ScrollX")
                           (hunchentoot:post-parameter "ScrollY")))
                 
                 )))
         (html-string:html-string
          `(:html
            (:head
             ((:meta :http-equiv "content-type" :content "text/html; charset=UTF-8"))
             ,@(cdr head)
             ((:script :language "javascript")
              "function saveScroll()"
              "{"
              "    document.Form1.ScrollX.value = document.body.scrollLeft;"
              "    document.Form1.ScrollY.value = document.body.scrollTop;"
              "}"
              ))
            (:body
             (:div
              ,shtml2))))))))
      
(defmacro define-ht-org (org-file uri
                         &key (title nil) (default-expand t) (return-uri nil)
                         (inline-image nil))
  `(let* ((org (read-org-file ,org-file))
          (org-shtml (org-to-shtml-w-head org (or ,title (pathname-name ,org-file)))))
     (define-ht-org-dispatcher ,uri org-shtml
       :default-expand ,default-expand
       :return-uri ,return-uri
       :inline-image ,inline-image
       :default-path ,org-file)))

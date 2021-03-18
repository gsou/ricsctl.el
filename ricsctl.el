;;; ricsctl.el --- Interact with a rics server from emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Skami
;;
;; Author: Skami <http://github/skami>
;; Maintainer: Skami
;; Created: March 15, 2021
;; Modified: March 15, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/skami/ricsctl
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This file provides the user with ways to aquire, send and plot
;; data from a rics server and to control the server.
;;
;;; Code:
(require 'cl-lib)
(require 'transient)
(require 'calc)


(defvar rics-ricsctl-cmd "ricsctl"
  "The command to call when starting a ricsctl process.")

(defvar rics-predicates nil
  "A list of the registered predicates.")

(defvar rics-id-format-hex t
  "Format the id in the csv as hex instead of as decimal.")

(defvar rics-data-format-hex t
  "Format the data in the csv as hex instead of as decimal.")

(defvar *rics-state* nil
  "Used internally by the predicates as state")

(defun rics-add-svr-args (args)
  "Prepend the list of arguments given ARGS with the required serve options."
  (append (rics-get-server-cmd (rics-get-option-value "--server")) args))

(defmacro rics-with-rustlog (f)
  "Set the RUST_LOG env var for the enclosed code F."
  (let ((ret (gensym)))
    `(progn (setenv "RUST_LOG" (rics-get-option-value "--RUST_LOG"))
            (let ((,ret (progn ,f)))
              (setenv "RUST_LOG")
              ,ret))))
(cl-defun rics-get-ricsctl (args &key (outbuf t outbufp))
  "Run ricsctl with the provided context and ARGS and return its stdout to the OUTBUF."
  (rics-with-rustlog
   (with-temp-buffer
     (apply 'call-process rics-ricsctl-cmd nil outbuf nil (rics-add-svr-args args))
     (buffer-string))))

(defun rics-make-ricsctl (args)
  "Make a ricsctl command list with args ARGS."
  (cons rics-ricsctl-cmd (rics-add-svr-args args)))

(defun rics-get-server-cmd (svr)
  "Get the command to the server SVR."
  (if (and svr (> (length svr) 0))
      (if (or (equal 'windows-nt system-type) (cl-search ":" svr))
          ;; TCP
          (list "--tcp" svr)
          ;; UDS
          (list "--uds" svr))))

(defun rics-get-option-value (name)
  "Get the option NAME for options in the format name=value."
  (let ((op (cl-find-if #'(lambda (v) (s-prefix-p name v)) (transient-args transient-current-command))))
    (if op
        (let ((val-pos (string-match "=" op)))
          (if val-pos
              (substring op (+ 1 val-pos))
            t)))))

(defun rics-command-out (str)
  "Perform output of the command STR depending of the given options."
  (if (string= "" str)
      (message "Command returned with no output.")
    (message str)))

;;; rics-control

(transient-define-argument rics-arg-server ()
  "Server to connect to."
  :description "Server"
  :class 'transient-option
  :shortarg "-s"
  :argument "--server=")

(transient-define-argument rics-arg-rustlog ()
  "Select the debug level used if a ricsctl process is started"
  :description "RUST_LOG debug level"
  :class 'transient-option
  :shortarg "-v"
  :argument "--RUST_LOG="
  :choices '("ERROR" "WARN" "INFO" "DEBUG" "TRACE"))

(transient-define-suffix rics-list-nodes ()
  "List the nodes on the current ricsctl server"
  :description "List nodes"
  (interactive)
  (message (rics-get-ricsctl '("list"))))

(transient-define-suffix rics-start-gui ()
  "Start the ricsctl gui application."
  :description "GUI"
  (interactive)
  (rics-get-ricsctl '("gui") :outbuf 0))

(transient-define-suffix rics-set-can-broadcast ()
  "Set the can broadcast flag of the server"
  :description "Set CAN broadcast flag"
  (interactive)
  (rics-command-out (rics-get-ricsctl '("can" "broadcast" "true"))))
(transient-define-suffix rics-clear-can-broadcast ()
  "Clear the can broadcast flag of the server"
  :description "Clear CAN broadcast flag"
  (interactive)
  (rics-command-out (rics-get-ricsctl '("can" "broadcast" "false"))))
(transient-define-suffix rics-delete-tmprics ()
  "Delete the /tmp/rics.socket file"
  :description "Delete /tmp/rics.socket"
  :transient t
  (interactive)
  (delete-file "/tmp/rics.socket")
  (message "/tmp/rics.socket deleted"))
(transient-define-suffix rics-pkill-ricsctl ()
  "Run pkill ricsctl."
  :description "pkill ricsctl"
  (interactive)
   (with-temp-buffer
     (call-process "pkill" nil t nil rics-ricsctl-cmd)
     (message (buffer-string))))
(transient-define-suffix rics-stop-server ()
  "Stop the server"
  :description "Stop the server"
  (interactive)
  (rics-command-out (rics-get-ricsctl '("stop"))))
;;(transient-define-suffix rics-start-server ()
;;  "Start a new server"
;;  :description "Start a new server"
;;  (interactive)
;;  (make-process :name "rics-server" :buffer "<rics-server>" (rics-make-ricsctl '("start"))))

(transient-define-suffix rics-print-ricsctl-version ()
  "Display the ricsctl version"
  :description "ricsctl version"
  (interactive)
  (rics-command-out (rics-get-ricsctl '("--version"))))
(transient-define-argument rics-can-send-id ()
  "Select the can id with which to send the message"
  :description "CAN Id"
  :class 'transient-option
  :shortarg "-i"
  :argument "--id=")
(transient-define-argument rics-can-send-data ()
  "Select the can data with which to send the message"
  :description "CAN Data"
  :class 'transient-option
  :shortarg "-d"
  :argument "--data=")
(transient-define-suffix rics-can-send-one ()
  "Send a can message to the server"
  :description "Send"
  (interactive)
  (let ((canid (rics-get-option-value "--id"))
        (data (rics-get-option-value "--data")))
    (if canid
        (if data
            (rics-command-out (rics-get-ricsctl `("can" "send" "--id" ,canid "--data" ,data)))
          (message "Data is not defined"))
      (message "Id is not defined")))
  ;; (rics-can-send)
  )
(transient-define-suffix rics-can-log-on (buf)
  "Log can messages in a buffer"
  :description "Log can messages"
  (interactive "BLog to:")
  (make-process :name "rics-can-log" :buffer buf :command (rics-make-ricsctl '("can" "log"))))

(transient-define-prefix rics-can-send ()
  "Send a can message on the selected server"
  ["Params"
   ("i" rics-can-send-id)
   ("d" rics-can-send-data)]
  ["Command"
   ("s" rics-can-send-one)])

(transient-define-prefix rics-control ()
  "Access the status of the current server or connect as a client"
  ["Arguments"
   ("v" rics-arg-rustlog)
   ("s" rics-arg-server)

]
  ["Server"
   ("V" rics-print-ricsctl-version)
   ;; TODO ("S S" rics-start-server)
   ("S C-s" rics-stop-server)
   ;; TODO route (subtransient: -d source node dest nodes)
   ;; TODO start luaplugin
   ("n" rics-list-nodes)
   ("l" rics-can-log-on)
   ("b" rics-set-can-broadcast)
   ("B" rics-clear-can-broadcast)
   ("c" "Send CAN message" rics-can-send)
   ;; TODO can sendall current buffer
   ("g" rics-start-gui)
   ]

  ["Misc"
   ("C-d" rics-delete-tmprics)
   ("C-k" rics-pkill-ricsctl)])


;;; rics-data

(defun rics-date-to-ms (str &optional offset)
  "Convert the string STR in format date.ms to a float. The OFFSET parameter specifies the origin."
  (- (let* ((dt (s-split "\\." str))
            (ms (+ (* 1000 (time-convert (date-to-time (car dt)) 'integer))
                   (floor (* 1000 (string-to-number (concat "0." (cl-second dt))))))))
       ms)
     (if (rics-get-option-value "--timestamp-normalise") (or offset 0) 0)))

(defun rics-push-ms-formatted(invec)
  "Convert the ms calc vector INVEC to the correct format, and push it to the stack."
  (calc-push invec)
  (pcase (rics-get-option-value "--timestamp-format")
    ;; ("ms" (calc-push invec))
    ("s"
     (calc-push 1000)
     (calc-map '( 2 calcFunc-div "/")))
    ("min"
     (calc-push 60000)
     (calc-map '(2 calcFunc-div "/")))
    ("hr"
     (calc-push 3600000)
     (calc-map '(2 calcFunc-div "/")))
    (_ nil)))


(defun rics-parse (str)
  "Parse a string into a can message structure"
  (let* ((m (split-string str ","))
         (ts (nth 0 m))
         (id (string-to-number (nth 1 m) (if rics-id-format-hex 16 10)))
         (l (string-to-number (nth 2 m)))
         (dvec (make-vector l 0))
         (data (cdddr m)))
    (dotimes (i l)
      (setf (aref dvec i) (string-to-number (car data) (if rics-data-format-hex 16 10)))
      (setq data (cdr data)))
    `(:time ,ts :id ,id :len ,l :data ,dvec)))

(defun rics-print-csv (mess)
  "Formats a message into a string"
  (format (concat "%s," (if rics-id-format-hex "%x" "%d") ",%s,%s\n") (cl-getf mess :time) (cl-getf mess :id) (cl-getf mess :len)
          (mapconcat #'(lambda (x) (format (if rics-data-format-hex "%x" "%d") x)) (cl-getf mess :data) ",")))

(defun rics-parse-region (ptmin ptmax)
  "Parse the region as a list of can message structure. Skips malformed items."
  (save-excursion
    (goto-char ptmin)
    (remove nil
            (cl-loop while (< (point) ptmax) collect
                     (let* ((startpos (point))
                            (endpos (min ptmax (line-end-position)))
                            (line (buffer-substring-no-properties startpos endpos)))
                       (forward-line 1)
                       (rics-parse line))))))


(defun rics-parse-buffer (&optional buf)
  "Parse the buffer BUF as a list of can message structures. Skips malformed items."
  (with-current-buffer (or buf (current-buffer))
    (rics-parse-region (point-min) (point-max))))

;; XXX Optional buffer argument for output buffer
(defun rics-filter-region (ptmin ptmax pred &optional dest)
  "Filter the region with the given predicate PRED."
  (interactive "r\naPredicate:\ni")
  (save-excursion
    (let ((reg (rics-parse-region ptmin ptmax)))
      (unless dest (kill-region ptmin ptmax))
      (goto-char ptmin)
      (setq *rics-state* nil)
      (dolist (v reg)
        (with-current-buffer (get-buffer-create (or dest (current-buffer)))
          (when (funcall pred v) (insert (rics-print-csv v))))))))
(defun rics-filter-buffer (pred &optional buf dest)
  "Filter the region with the given predicate PRED."
  (interactive "aPredicate:\nbSource:\nBDest:")
  (with-current-buffer (or buf (current-buffer))
    (rics-filter-region (point-min) (point-max) pred dest)))
(defun rics-filter-line (pred)
  "Filter the current line with the given predicate PRED."
  (interactive "aPredicate:")
  (rics-filter-region (line-beginning-position) (line-end-position) pred))


(defun rics-extract-region (ptmin ptmax pred &optional buf )
  "Extract the data from the predicate PRED to the buffer BUF."
  (interactive "r\naPredicate:\ni")
  (save-excursion
    (let ((reg (rics-parse-region ptmin ptmax))
          (timestampp (rics-get-option-value "--timestamp-format")))
      (unless buf (kill-region ptmin ptmax))
      (goto-char ptmin)
      (if (equal buf 'calc)
          (progn
            (setq *rics-state* nil)
            (let ((dat (remove nil (mapcar pred reg))))
              (when timestampp
                (setq *rics-state* nil)
                (let* ((offset (rics-date-to-ms (cl-getf (cl-first reg) :time)))
                       (ts (remove nil (mapcar (lambda (x) (if (funcall pred x) (rics-date-to-ms (cl-getf x :time) offset))) reg))))
                  (rics-push-ms-formatted (cons 'vec ts))))
              (calc-push (cons 'vec dat))))
        (progn
          (setq *rics-state* nil)
          (dolist (v reg)
            (let ((res (funcall pred v)))
              (with-current-buffer (get-buffer-create (or buf (current-buffer)))
                (when res (insert (format "%s,%s\n" (cl-getf v :time) res)))))))))))
(defun rics-extract-buffer (pred &optional buf dest)
  "Extract the data from the predicate PRED."
  (interactive "aPredicate:\nbSource:\nBDest:")
  (with-current-buffer (or buf (current-buffer))
    (rics-extract-region (point-min) (point-max) pred dest)))
(defun rics-extract-region-calc (ptmin ptmax pred)
  "Extract the data in the region PTMIN PTMAX from the predicate PRED and push the timestamp and the data in two GNU Calc vectors."
  (interactive "r\naPredicate:")
  (rics-extract-region ptmin ptmax pred 'calc))
(defun rics-extract-buffer-calc (pred &optional buf)
  "Extract the data in the buffer BUF from the predicate PRED and push the timestamp and the data in two GNU Calc vectors."
  (interactive "aPredicate:\ni")
  (with-current-buffer (or buf (current-buffer))
    (rics-extract-region (point-min) (point-max) pred 'calc)))



(defmacro rics-register-predicate (name &rest body)
  "Register the predicate F with name NAME so it can be used as an input for the filter/extract/plot functions."
  `(progn (push ,name rics-predicates)
          (defalias (intern (concat "rics-predicate-" ,name)) (lambda (item) (let ((id (cl-getf item :id))
                                                                                    (len (cl-getf item :len))
                                                                                    (data (cl-getf item :data))
                                                                                    (time (cl-getf item :time)))
                                                                                ,@body)))))
(defmacro rics-register-uint8 (name id mask offset)
  "Register a predicate named NAME with a uint8_t in message ID with offset OFFSET."
  `(rics-register-predicate ,name
    (if (= (logand ,mask id) ,id) (elt data ,offset))))
(defmacro rics-register-int8 (name id mask offset)
  "Register a predicate named NAME with a int8_t in message ID with offset OFFSET."
  `(rics-register-predicate ,name
                            (if (= (logand ,mask id) ,id) (let ((d (elt data ,offset)))
                                             (if (> d 128) (- 256 d) d)))))
(defmacro rics-register-uint16 (name id mask offset &optional be)
  "Register a predicate named NAME with a uint16_t in message ID with offset OFFSET."
  `(rics-register-predicate ,name
                            (if (= (logand ,mask id) ,id) (+ (* (if ,be 256 1) (elt data ,offset))
                                              (* (if ,be 1 256) (elt data ,(+ 1 offset)))))))
(defmacro rics-register-int16 (name id mask offset &optional be)
  "Register a predicate named NAME with a int16_t in message ID with offset OFFSET."
  `(rics-register-predicate ,name
                            (if (= (logand ,mask id) ,id) (let ((d (+ (* (if ,be 256 1) (elt data ,offset))
                                                       (* (if ,be 1 256) (elt data ,(+ 1 offset))))))
                                             (if (> d 32767) (- 65536 d) d)))))


;; TODO Live functions (live plot, live show data, etc)

;(transient-define-argument rics-arg-filter-id ()
;  "Filter the ID of the message."
;  :description "Filter id"
;  :class 'transient-option
;  :shortarg "-i"
;  :argument "--id=")

;; Transient data iface

(defun rics-gen-predicate-str (str)
  "Generate a predicate from the string STR. Prefer a predicate defined in rics-predicates."
  (intern (concat "rics-predicate-" str)))

(transient-define-argument rics-arg-predicate ()
  "Select the predicate to be used."
  :description "Predicate"
  :class 'transient-option
  :shortarg "-p"
  :argument "--predicate="
  :choices #'(lambda (_a _b _c) rics-predicates))
(transient-define-argument rics-arg-sourcebuffer ()
  "Select the source buffer for the action"
  :description "Source buffer"
  :class 'transient-option
  :shortarg "-b"
  :argument "--buffer="
  :choices #'(lambda (_a _b _c) (mapcar #'buffer-name (buffer-list))))
(transient-define-argument rics-arg-targetbuffer ()
  "Select the target buffer for the action"
  :description "Target buffer"
  :class 'transient-option
  :shortarg "-t"
  :argument "--target="
  ;; :choices #'(lambda () (mapcar #'buffer-name (buffer-list)))
  )
(transient-define-argument rics-arg-timestamp-format ()
  :description "Format timestamp as"
  :class 'transient-switches
  :argument-format "--timestamp-format=%s"
  :argument-regexp "--timestamp-format"
  :choices '("ms" "s" "min" "hr"))

(transient-define-suffix rics-filter-function ()
  "Apply the filter"
  :description "Filter"
  (interactive)
  (let ((pred (rics-gen-predicate-str (rics-get-option-value "--predicate")))
        (source (rics-get-option-value "--buffer"))
        (target (rics-get-option-value "--target")))
    (if (and (use-region-p) (not source))
        (rics-filter-region (region-beginning) (region-end) pred target)
      (rics-filter-buffer pred source target))))
(transient-define-suffix rics-extract-function ()
  "Apply the filter"
  :description "Extract"
  (interactive)
  (let ((pred (rics-gen-predicate-str (rics-get-option-value "--predicate")))
        (source (rics-get-option-value "--buffer"))
        (target (rics-get-option-value "--target")))
    (if (and (use-region-p) (not source))
        (rics-extract-region (region-beginning) (region-end) pred target)
      (rics-extract-buffer pred source target))))
(transient-define-suffix rics-calc-function ()
  "Move data to calc"
  :description "Move to Calc"
  (interactive)
  (let ((pred (rics-gen-predicate-str (rics-get-option-value "--predicate")))
        (source (rics-get-option-value "--buffer")))
    (if (and (use-region-p) (not source))
        (rics-extract-region-calc (region-beginning) (region-end) pred)
      (rics-extract-buffer-calc pred source))))
(transient-define-suffix rics-add-plot-function ()
  "Add a plot of the data"
  :description "Add data to plot"
  (interactive)
  (rics-calc-function)
  (calc-graph-add nil)
  (calc-graph-name (rics-get-option-value "--predicate"))
  (calc-pop 2))
(transient-define-suffix rics-parse-function (var)
  "Parse to a variable."
  :description "Parse to variable"
  (interactive "STarget:")
  (set var (let ((source (rics-get-option-value "--buffer")))
             (if (and (use-region-p) (not source))
                 (rics-parse-region (region-beginning) (region-end))
               (rics-parse-buffer source)))))
(transient-define-suffix rics-plot-this-function ()
  "Plot the given predicate"
  :description "Plot this function with calc"
  (interactive)
  (rics-add-plot-function)
  (calc-graph-plot 0))

(transient-define-prefix rics-data ()
  "Process rics data"
  ["Timestamp"
   ("=" rics-arg-timestamp-format)
   ("N" "Normalise timestamp to first input" "--timestamp-normalise")]
  ["Parameters"
   ("p" rics-arg-predicate)
   ("b" rics-arg-sourcebuffer)
   ("t" rics-arg-targetbuffer)]
  ["Actions"
   ("P" rics-parse-function)
   ("a" rics-add-plot-function)
   ("g" rics-plot-this-function)
   ("f" rics-filter-function)
   ("d" rics-extract-function)
   ("c" rics-calc-function)])


(global-set-key (kbd "C-c R") 'rics-control)
(global-set-key (kbd "C-c r") 'rics-data)


(provide 'ricsctl)
;;; ricsctl.el ends here

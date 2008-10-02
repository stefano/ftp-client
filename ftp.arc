(= eol* (string #\newline #\return))

(= ftp-block-eof* 64)

(= parse-resp (seq (n-times (one-of "0123456789") 3) (one-of "- ")))

(= parse-digits (many1 (one-of "0123456789")))
(= parse-coma (char #\,))

(= parse-pasv-port 
  (seq (char #\() parse-digits parse-coma parse-digits  parse-coma parse-digits parse-coma parse-digits parse-coma parse-digits  parse-coma parse-digits (char #\))))

(def ftp-resp-code (in-s)
  "get the response (code . response) or nil"
  (let line (trim (readline in-s) 'end #\return)
    (when (> (len line) 2)
      (awhen (parse-resp line 0)
        (if (is (cadr (car it)) " ") ; one-liner 
              (cons (reduce string (caar it)) line)
            (is (cadr (car it)) "-") ; multi-line
              (let code (reduce string (caar it))
                (cons code 
                      (string line #\newline (ftp-resp-upto in-s code)))))))))

(def ftp-resp-upto (in-s code (o acc ""))
  (let line (readline in-s)
    (if (and (>= (len line) 4) (is (cut line 0 4) (string code " ")))
      acc
      (ftp-resp-upto in-s code (string acc "\n" line)))))

(def ftp-all-resps (in-s)
  "get all responses"
  (rev:accum acc
    ((afn () (awhen (ftp-resp-code in-s)
               (acc it)
               (if (is ((car it) 0) #\1) ; continue
                 (self)))))))

(def ftp-mk-cmd (cmd)
  "create a function that executes a command and that returns a 
   list of responses"
  (fn (in-s out-s . args) 
    (w/stdout out-s
      (apply pr (upcase cmd) " " (when args (intersperse " " args)))
      (pr eol*)
      (flush-socket out-s)
      (ftp-all-resps in-s))))

(mac ftp-defcmds cmds
  `(do
     ,@(map [let name (sym:string 'ftp- _)
              `(= ,name (ftp-mk-cmd ,(string _)))]
            cmds)))

; acess control
(ftp-defcmds user pass cwd cdup quit)

; transfer parameters
(ftp-defcmds pasv port type mode)

; service commands
(ftp-defcmds retr stor pwd)


(def ftp-get-stream-mode (in-s)
  "read a file in STREAM mode, return it as a string"
  (tostring
    (whiler c (readb in-s) nil ; read upto EOF
      (writeb c))))

(def ftp-get-block-mode (in-s)
  "read a file in BLOCK mode, return it as a string"
  (tostring
    (let type nil
      (until (is type ftp-block-eof*)
        (let (tp block) (ftp-get-single-block in-s)
          (= type tp)
          (pr block))))))

(def ftp-get-single-block (in-s)
  "read a single block, return type of block and the block"
  ; read the header
  (with (desc (or (readb in-s) (err "Cannot read block header!"))
         count (+ (* (coerce (readb in-s) 'int) 256)
                  (coerce (readb in-s) 'int)))
    ; read the block
    (list (coerce desc 'int)
          (tostring
            (for i 1 count
              (aif (readb in-s)
                (writeb it)
                (err "Connection closed before full file transfer!")))))))

;(def ftp-pasv-retr (i o file (o mode 'stream))
;  "retrieve file in passive mode"
;  (awhen (ftp-pasv i o)
;    (let 

(mac ftp-w/session (in-s out-s host port . body)
  "execute body within an ftp section"
  (w/uniq (h p)
    `(withs (,h ,host ,p ,port)
       (let (,in-s ,out-s) (connect-socket ,h ,p)
         (protect (fn () (ftp-all-resps ,in-s) ,@body)
                  (fn () (close ,in-s) (close ,out-s)))))))

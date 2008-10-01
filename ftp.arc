(= eol* (string #\newline #\return))

(= ftp-block-eof* 64)

(= parse-resp (seq (n-times (one-of "0123456789") 3) (char #\space)))

(= parse-pasv-port 
   (with (n (n-times (one-of "0123456789") 3)
          c (char #\,))
     (apply seq (intersperse c (n-of n)))))

(def ftp-resp-code (line)
  "get the response code or nil"
  (when (> (len line) 2)
    (awhen (parse-resp line 0)
      (reduce string (caar it)))))

;(def ftp-resp-or-fail (in-s resp)
;  (let line (readline in-s)
;    (if (aand (ftp-resp-code line) (is it resp))
;      line
;      (err "Wrong response"))))

(def ftp-all-resps (in-s)
  "get all responses"
  ; to be implemented
  nil)

(def ftp-mk-cmd (cmd)
  "create a function that executes a command and that returns a 
   list of responses"
  (fn (in-s out-s . args) 
    (w/stdout out-s
      (apply pr (upcase cmd) " " (intersperse " " args))
      (pr eol*)
      (ftp-all-resps in-s))))

(mac ftp-defcmds cmds
  `(do
     ,@(map [let name (intern:string 'ftp- _)
              `(= ,name (ftp-mk-cmd ,(string _) ,(string expects)))))))

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

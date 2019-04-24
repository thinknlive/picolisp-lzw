#{
initialize TABLE[0 to 255] = code for individual bytes
CODE = read next code from encoder
STRING = TABLE[CODE]
output STRING
while there are still codes to receive:
  CODE = read next code from encoder
  if TABLE[CODE] is not defined: // needed because sometimes the
    ENTRY = STRING + STRING[0] // decoder may not yet have entry!
  else:
    ENTRY = TABLE[CODE]
  output ENTRY
  add STRING+ENTRY[0] to TABLE
  STRING = ENTRY
}#

#{
initialize TABLE[0 to 255] = code for individual bytes
STRING = get input symbol
while there are still input symbols:
  SYMBOL = get input symbol
  if STRING + SYMBOL is in TABLE:
    STRING = STRING + SYMBOL
  else:
    output the code for STRING
    add STRING + SYMBOL to TABLE
    STRING = SYMBOL
  output the code for STRING
}#


(de lzwCompress (Lst MaxN)
   (setq *LZWDict NIL)
   
   (let (Codes (if MaxN MaxN (apply max Lst)))
      (balance '*LZWDict
         (make
            (for C Codes
               (link (list (list C) C 0) ) ) ) )
      (make
         (let W (list (pop 'Lst))
            (for C Lst
               (let WC (append W (list C))
                  (if (lup *LZWDict WC)
                     (setq W WC)
                     (link (cadr (lup *LZWDict W)))
                     (inc (cddr (lup *LZWDict W)))
                     (idx '*LZWDict (list WC (inc 'Codes) 0) T)
                     (setq W (list C)) ) ) )         
            (and W (link (cadr (lup *LZWDict W)))) ) ) ) )
 
(de lzwDecompress (Lst MaxN)
   (ifn MaxN (throw "lzwDecompress: Data error; MaxN needed"))
   (setq *LZWDict NIL)
   (let (Codes MaxN)
      (balance
         '*LZWDict
         (make (for C Codes (link (list C C)))) )
      (make
         (let W NIL
            (for N Lst
               (let WC
                  (if (lup *LZWDict N)
                     (cdr @)
                     (cons (last W) W) )
                  (chain (reverse WC))
                  (when W
                     (idx
                        '*LZWDict
                        (cons (inc 'Codes) (cons (last WC) W))
                        T ) )
                  (setq W WC) ) ) ) ) ) )
									
# --------------------------------------------------------------------------------------------

(de lzwDecompress (Lst MaxN MaxCode)
   (ifn MaxN (throw "lzwDecompress: Data error; MaxN needed"))
   (ifn MaxCode (throw "lzwDecompress: Data error; MaxCode needed"))
   (setq *LZWDict NIL)
   (setq *LZWLen 0)
   (let
      (Codes NIL
         CodeCount 0
         CodesQueue NIL
         InitCodes
         '(NIL (setq Codes MaxN)
            (msg "#lzwDecompress: InitCodes")
            (let (Code (inc MaxN))
               (while (<= Code MaxCode)
                  (queue 'CodesQueue Code)
                  (inc 'Code) ) )
            (balance
               '*LZWDict
               (make
                  (for C Codes
                     (link (list C (list C) 0 (inc 'CodeCount)) ) ) ) )
            (msg (idx '*LZWDict))
            (msg CodesQueue) )
         PurgeCodes
         '(NIL
            (let (Dict (idx '*LZWDict)  Cnt 0)
               (setq Dict (by cadddr sort Dict))
               (for Elt Dict
                  (NIL (< Cnt 1))
                  (when Elt
                     (queue 'CodesQueue (car Elt))
                     (idx '*LZWDict Elt NIL)
                     (inc 'Cnt) ) )
               (if (not CodesQueue) (throw "#lzwDecompress: Codes exhausted")) )
             ) )
      (InitCodes)
      (make
         (let (W NIL  Code NIL  Lzw NIL)
            (for N Lst
               (let
                  (WC
                     (if (lup *LZWDict N)
                        (cadr @)
                        (cons (last W) W) ) )
                  (chain (reverse WC))
                  (when W
                     (ifn CodesQueue (PurgeCodes))
                     (setq Code (pop 'CodesQueue))
                     (idx
                        '*LZWDict
                        (list
                           Code
                           (cons (last WC) W)
                           0
                           (inc 'CodeCount) )
                        T ) )
                  (setq W WC) ) ) ) ) ) )

# --------------------------------------------------------------------------------------------
                  
(de lzwCompress (Lst MaxN MaxCode)
   (setq *LZWDict NIL)
   (setq *LZWLen 0)
   (default MaxCode 512)
   (default MaxN (apply max Lst))
   (let
      (Codes NIL
         CodeCount 0
         CodesQueue NIL
         InitCodes
         '(NIL (setq Codes MaxN)
            (let (Code (inc MaxN))
               (while (<= Code MaxCode)
                  (queue 'CodesQueue Code)
                  (inc 'Code) ) )
            (balance
               '*LZWDict
               (make
                  (for C Codes
                     (link (list (list C) C 0 (inc 'CodeCount))) ) ) ) )
         PurgeCodes
         '(NIL
            (let (Dict (idx '*LZWDict)  Cnt 0)
               (setq Dict (by cadddr sort Dict))
               (for Elt Dict
                  (NIL (< Cnt 1))
                  (when Elt
                     (queue 'CodesQueue (cadr Elt))
                     (idx '*LZWDict Elt NIL)
                     (inc 'Cnt) ) )
               (if (not CodesQueue) (throw "#lzwCompress: Codes exhausted")) ) ) )
      (InitCodes)
      (make
         (let (W (list (pop 'Lst))  Code NIL)
            (for C Lst
               (let WC (append W (list C))
                  (if (lup *LZWDict WC)
                     (setq W WC)
                     (unless (lup *LZWDict W)
                        (ifn CodesQueue (PurgeCodes))
                        (setq Code (pop 'CodesQueue))
                        (idx
                           '*LZWDict
                           (list W Code 0 (inc 'CodeCount))
                           T ) )
                     (link (cadr (lup *LZWDict W)))
                     (inc (cddr (lup *LZWDict W)))
                     (inc '*LZWLen (length (car (lup *LZWDict W))))
                     (ifn CodesQueue (PurgeCodes))
                     (setq Code (pop 'CodesQueue))
                     (idx
                        '*LZWDict
                        (list WC Code 0 (inc 'CodeCount))
                        T )
                     (setq W (list C)) ) ) )
            (and
               W
               (unless (lup *LZWDict W)
                  (ifn CodesQueue (PurgeCodes))
                  (setq Code (pop 'CodesQueue))
                  (idx
                     '*LZWDict
                     (list W Code 0 (inc 'CodeCount))
                     T ) )
               (link (cadr (lup *LZWDict W))) ) ) ) ) )

               
               

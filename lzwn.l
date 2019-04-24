(de lzwnCompress (Lst MaxN Commas)
    (setq *LZWNDict NIL)
    (ifn MaxN (throw "#lzwnCompress: error; MaxN required"))
    (ifn Commas (throw "#lzwnCompress: error; Commas require"))
    (if (atom Commas) (setq Commas (list Commas)))
    (ifn (lst? Commas) (throw "#lwznCompress: error; Commas not a list"))
    (let (W NIL WC NIL)
      (setq W (make
          (let (Elts NIL Codes MaxN)
            (for C Lst
              (setq Elts (cons C Elts))
              (ifn (index C Commas)
                (link C)
                (ifn (lup *LZWNDict Elts)
                  (prog 
                    (link C)
                    (idx '*LZWNDict (list Elts (inc 'Codes)) T) )
                  (prog
                    (setq WC (cons (cadr (lup *LZWNDict Elts)) WC)) ) )
                (setq Elts NIL) ) )
            (when Elts
              (ifn (lup *LZWNDict Elts)
                (chain (copy Elts))
                (setq WC (cons (cadr (lup *LZWNDict Elts)) WC)) ) ) ) ) )
      (setq WC (reverse WC))
      (list W WC)))
 
(de lzwnDecompress (Lst MaxN)
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



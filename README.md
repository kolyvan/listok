## Listok is a dialect of LISP 
Useful as an embedded scripting language or as stand-alone app.
It's a Scala-based and need in scala-library.jar to run.


## features

- lisp-1 
- lexical scoping 
- tail-call optimization
- regex and pattern matching
- concurrency via threads and mailslots
- lazy sequences
- java interop

## types

- nil, t
- int, float, bignum, ratio
- char, symbol, keyword, pair, byte, blob
- string, list, vector, hashmap, lazyseq	
- hashtable
- stream 
- regex
- thread, mailslot, process, connection
- function
	

## special forms

- def, defconstant, defun, defstruct, setf,
- let, let*
- if, cond, do, and, or
- spawn, match
- collect
- assert


## macro
	
- defmacro, macroexpand, gensym	
	
		
## built-in macros
	
- progn, dotimes, doseq, dowhile, loop, when, unless
	
 
## built-in function
 
### common
 
- eq, not, eval, apply, load, exit, error, 
- break, trace, untrace
- curry, current-time, current-directory, json-parse
- display, format 

- gensym, macroexpand

- to-str, to-char, to-int, to-float, to-number, to-list, to-vector

- atom, listp, null, sequencep, keywordp, numberp, symbolp, functionp
- stringp, vectorp, charp, streamp, regexp, bytep, blobp
- hashtablep, hashmapp threadp, mailslotp, structp, lazyseqp, pairp
 
### number
	
- \+, \-, \/, \*, <, <=, >, >=, =, \/=, 
- incr, decr, rem, oddp, evenp, zerop, plusp, minusp
- min, max, abs, gcd, expt
 
### sequences

- list, vector, string, range, pair		
- length, head, first, last, tail rest
- cons, append, reverse, elt, set-elt, subseq,
-	find, find-if, position, position-if, count, count-if,
- remove, remove-if, sort, take, drop, 
-	foreach, reduce, fold, map, flatmap, filter, partition,
    
- hashmap, hm-get, hm-add, hm-del
- hash-table, gethash, sethash, remhash, clrhash, maphash

- lazyseq, lazyseq-from, lazyseq-range, lazyseq-force
 
### streams

- open, open-tcp-connection, open-http-connection, close
- read, read-byte, read-blob, read-char, read-text, read-line,
- print, write, write-byte, write-blob, write-char, write-string, write-line,
- terpri, newline,
- make-string-input-stream, make-string-output-stream,
- make-blob-input-stream, make-blob-output-stream,
- get-output-stream-string, get-output-stream-blob
- url-encode
- get-output-stream, get-input-stream
- open-stream-p, input-stream-p, output-stream-p
   
### regex

- regex, regex-matches, regex-find, regex-find-all,
- regex-scan, regex-split, regex-replace
  
  
### concurrent

- join, thread-wait-init, sleep 
- mailslot, receive, send
- process, process-exit-value, process-wait,
- process-output-stream, process-input-stream, process-error-stream

## sample code

###  pattern mathing and regex

    (match s
      ((#/(\w+):(\d+)/ name id) 
        (display "name=%s id=%s\n" name id)))


### thread and mailslot

    (let ((x 1))
      (spawn worker ((x) (m (mailslot)))
          (send m (+ 1 x))))
    (first (receive)) ; 2


### tco  

    (defun evenp (n)
      (declare :tco)
      (if (eq n 0)
        t
        (oddp (- n 1))))

    (defun oddp (n)
      (declare :tco)
      (if (eq n 0)
        nil
        (evenp (- n 1))))

    (evenp 10002)


### lazyseq

    (defun sieve (s)
      (lazyseq (head s)
        (lambda (n)
          (sieve
            (filter
              (lambda (x) (/= 0 (rem x n)))
              (tail s))))))

    (def primes (sieve (lazyseq-from 2)))  
    (to-list (take primes 10))


### collect

    (def s (open (string (current-directory) "/src/test/resources/test.file" )))
    (assert s)
    (print (collect (dowhile (<< (read-line s)))))
    (close s)

### interop

    (interop "java.io.File" 'make-file :constructor)
    (interop "java.io.File.getAbsolutePath" 'file/path)
    (file/path (make-file "."))


## limitation

- in argument lists no &key or &options but only &rest parameter
- in macro definition use backquote instead of quote


## planned in nearest future

- unwind-protect form and with-open macro


## Build instructions

Listok uses sbt to build.  
Get sbt at http://code.google.com/p/simple-build-tool/

To build:
   
	cd listok
	sbt update test package
    
To run REPL:
    
	cd examples
	./listok
    
To run examples:
		
	cd examples
	./queens.lok all
	./gtranslate.lok en ru hello
	./digest.lok digest.lok sha
	./test.lok 256
	./traffic.lok 100

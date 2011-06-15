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

## types

- nil, t
- int, float
- char, symbol, keyword, pair
- string, list, vector, hashmap, lazyseq	
- hashtable
- stream 
- regex
- thread, mailslot, process
- function
	

## special forms

- def, defconstant, defun, defstruct, setf, 
- if, cond, do, and, or
- spawn, match
 

## macro
	
- defmacro, macroexpand, gensym	
	
		
## built-in macros
	
- progn, let, dotimes, doseq, dowhile, loop, when, unless
	
 
## built-in function
 
### common
 
- eq, not, eval, apply, load, exit, error, 
- break, assert, trace, untrace
- curry, current-time, current-directory, json-parse
- display, format 

- gensym, macroexpand

- to-str, to-char, to-int, to-float, to-list, to-vector

- atom, listp, null, sequencep, keywordp, numberp, symbolp, functionp,
- stringp, vectorp, charp, streamp, regexp, 
- hashtablep, hashmapp threadp, mailslotp, structp, lazyseqp, pairp
 
### number
	
- \+, \-, \/, \*, <, <=, >, >=, =, \/=, 
- incr, decr, rem, oddp, evenp, zerop, plusp, minusp
- min, max, abs
 
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

- open, open-socket, open-url, close,
- read, read-char, read-text, read-line,
- print, write, write-char, write-string, write-line,
- terpri, newline,
- make-string-input-stream, make-string-output-stream,
- get-output-stream-string, 
- url-encode
   
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



## limitation

- in argument lists no &key or &options but only &rest parameter
- in macro definition use backquote instead of quote
- unable to use `def` inside `do` forms

## planned in nearest future

- generic methods
- values
- numerical tower (byte, double, long, etc)

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

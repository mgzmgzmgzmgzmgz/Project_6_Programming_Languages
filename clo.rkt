#lang racket    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
; 
; Project 6 -- closure conversion -- implementing first-class functions 
; 
; The goal of this project is to write a functional compiler pass for bottom-up closure conversion. 
; Bottom-up closure conversion implements lambdas by turning them into a top-level definition	
; of a non-higher-order (C-style) function and a closure-creation point that pairs this function    
; with values for its free variables in a vector. Each (lambda (x y) ...) should turn into a 
; top-level proc (proc (lam5 env5 x y) (let ([z (vector-ref env5 1)]) ...)) with an added parameter	
; for its environment to be passed to it (this example shows one free variable, z), and a closure	  
; (vector lam5 z) which saves this top-level function-pointer together with its free variables in some order.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide prim?	
         closure-convert) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    
; Honor pledge (please write your name.)    
;
; I **firstname lastname** have completed this code myself, without    
; unauthorized assistance, and have followed the academic honor code.	  
;
; Edit the code below to complete your solution.    
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    



;;; Input language; CPS    
; ce ::= (let ([x (prim-op ae ...)]) ce)    
;     |  (ae ae ...)
;     |  (if ae ce ce)	  
; ae ::= x | n | (lambda (x ...) ce)
; x is a symbol?  (possibly 'halt)
; n is a number?    
; prim-op is a prim? 


(define (prim? op)	  
  (if (member op '(< = <= + - * vector vector-ref))
      #t	  
      #f))	  


(define (closure-convert e)    
  ; Bottom up takes an expression and current list of procs, and returns a list	  
  ; `(,e+ ,freee+ ,procs+) of the converted expression, its free variables, and a possibly-updated list of procs    
  (define (bottom-up e procs)
    (match e	  
           [`(let ([,x ,rhs]) ,bdy)    
            (match-define `(,rhs+ ,free0 ,procs0)	  
                          (bottom-up rhs procs))	  
            (match-define `(,bdy+ ,free1 ,procs1)
                          (bottom-up bdy procs0))
            `((let ([,x ,rhs+]) ,bdy+) 
              ,(set-remove (set-union free0 free1) x)
              ,procs1)]    
            ; TODO: Add your other cases here	
           )) 
  ; Use bottom-up to generate a main procedure from the program expression e 
  (match-define `(,main-body ,freevars ,procs)    
                (bottom-up e '())) 
  `((proc (main) ,main-body) . ,procs)) 


;;; Output Language	  
; prog ::= proc ...  eg -> (proc (main) (clo-app (vector lam3 ...) ...)) (proc (lam3 ...))
; proc ::= (proc (x ...) ce)	
; ce ::= (let ([x (prim-op ae ...)]) ce)	  
;     |  (clo-app ae ae ...)    
;     |  (if ae ce ce) 
; ae ::= x | n | (prim-op ae ...)
; x is a symbol?  (possibly 'halt) 
; n is a number?	  
; prim-op is a prim?


    



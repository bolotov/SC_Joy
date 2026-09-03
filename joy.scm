(import (rnrs)
        (rnrs hashtables)
        (rnrs mutable-pairs)
        (only (chezscheme) format command-line-arguments))

;;; ==========================================================================
;;; MARK: Core Joy Environment
;;; ========Global symbol environment and data stack for Joy programs=========

;;; Global environment and data deque
(define joy-env (make-eq-hashtable))
(define joy-stack (cons '() '())) ;; (top-half . bottom-half)
(define joy-mode 'stack)

;;; Raise a Joy-specific error with a message
(define (joy-error . args)
  (error 'joy (apply string-append args)))

;;; Result pattern for portable error handling
(define (joy-ok val) (cons 'ok val))
(define (joy-err msg) (cons 'err msg))
(define (joy-ok? res) (and (pair? res) (eq? (car res) 'ok)))
(define (joy-err? res) (and (pair? res) (eq? (car res) 'err)))
(define (joy-res-val res) (cdr res))

;;; Environment operations
(define (joy-get sym)
  (hashtable-ref joy-env sym '()))

(define (joy-set! sym val)
  (hashtable-set! joy-env sym val))

;;; Stack operations
(define (joy-push! x)
  (set-car! joy-stack (cons x (car joy-stack))))

(define (joy-pop!)
  (cond
    ((pair? (car joy-stack))
     (let ((top (caar joy-stack)))
       (set-car! joy-stack (cdar joy-stack))
       (joy-ok top)))
    ((pair? (cdr joy-stack))
     (set-car! joy-stack (reverse (cdr joy-stack)))
     (set-cdr! joy-stack '())
     (joy-pop!))
    (else (joy-err "Stack underflow"))))

(define (joy-peek)
  (cond
    ((pair? (car joy-stack)) (joy-ok (caar joy-stack)))
    ((pair? (cdr joy-stack))
     (set-car! joy-stack (reverse (cdr joy-stack)))
     (set-cdr! joy-stack '())
     (joy-peek))
    (else (joy-err "Stack underflow on peek"))))

(define (joy-stack-size)
  (+ (length (car joy-stack)) (length (cdr joy-stack))))

(define (joy-clear-stack!)
  (set-car! joy-stack '())
  (set-cdr! joy-stack '()))

(define (joy-push-list! lst)
  (for-each joy-push! (reverse lst)))

;;; Deque-mode extension
(define (joy-unshift! x)
  (set-cdr! joy-stack (cons x (cdr joy-stack))))

(define (joy-shift!)
  (cond
    ((pair? (cdr joy-stack))
     (let ((bot (cadr joy-stack)))
       (set-cdr! joy-stack (cddr joy-stack))
       bot))
    ((pair? (car joy-stack))
     (set-cdr! joy-stack (reverse (car joy-stack)))
     (set-car! joy-stack '())
     (joy-shift!))
    (else (joy-error "Deque underflow"))))

;;; ===========================================================================
;;; MARK: Tokenization and Parsing
;;; ===========================================================================

;;; Convert a raw token string into a Scheme object:
;;; number, boolean, symbol, character or string
(define (joy-parse-token tok)
  (cond
    ((string=? tok "[") 'open-bracket)
    ((string=? tok "]") 'close-bracket)
    ((string=? tok ".") '.dot)
    ((and (> (string-length tok) 1) (char=? (string-ref tok 0) #\'))
     (let ((content (substring tok 1 (string-length tok))))
       (cond
         ((string=? content "\\n") #\newline)
         ((string=? content "\\t") #\tab)
         ((string=? content "\\r") #\return)
         ((string=? content "\\b") #\backspace)
         ((and (> (string-length content) 1) (char=? (string-ref content 0) #\\) (char-numeric? (string-ref content 1)))
          (let ((code (string->number (substring content 1 (string-length content)) 8)))
            (if code (integer->char code) (string->symbol tok))))
         ((= (string-length content) 1) (string-ref content 0))
         (else (string->symbol tok)))))
    ((and (> (string-length tok) 1) (char=? (string-ref tok 0) #\"))
     (substring tok 1 (- (string-length tok) 1)))
    ((string->number tok) => values)
    ((string=? tok "true") #t)
    ((string=? tok "false") #f)
    (else (string->symbol tok))))

;;; Convert a flat token list into a Joy expression list,
;;; properly grouping quotations with brackets
(define (joy-parse-tokens tokens)
  (let loop ((tokens tokens) (result '()) (quote-stack '()))
    (cond
      ((null? tokens)
        (if (null? quote-stack)
          (reverse result)
          (joy-error "Unmatched opening bracket")))
      ((eq? (car tokens) 'open-bracket)
        (loop (cdr tokens) result (cons '() quote-stack)))
      ((eq? (car tokens) 'close-bracket)
        (if (null? quote-stack)
          (joy-error "Unmatched closing bracket")
          (let ((current-quote (car quote-stack))
                (remaining-stack (cdr quote-stack)))
            (if (null? remaining-stack)
              (loop (cdr tokens)
                (cons (reverse current-quote) result)
                '())
              (loop (cdr tokens)
                result
                (cons (cons (reverse current-quote)
                        (car remaining-stack))
                  (cdr remaining-stack)))))))
      (else
        (if (null? quote-stack)
          (loop (cdr tokens)
            (cons (car tokens) result)
            quote-stack)
          (loop (cdr tokens)
            result
            (cons (cons (car tokens) (car quote-stack))
              (cdr quote-stack))))))))

;;; Split a character list into token strings.
;;; Handles comments (* *), # and delimiters [ ] .
(define (joy-split chars)
  (let loop ((chars chars) (tokens '()))
    (cond
      ((null? chars) (reverse tokens))
      ((char-whitespace? (car chars)) (loop (cdr chars) tokens))
      ((char=? (car chars) #\#) ;; line comment
       (loop (drop-until-newline (cdr chars)) tokens))
      ((and (char=? (car chars) #\() (not (null? (cdr chars))) (char=? (cadr chars) #\*))
       (loop (drop-comment (cddr chars)) tokens))
      ((char=? (car chars) #\[) (loop (cdr chars) (cons "[" tokens)))
      ((char=? (car chars) #\]) (loop (cdr chars) (cons "]" tokens)))
      ((char=? (car chars) #\.) (loop (cdr chars) (cons "." tokens)))
      ((char=? (car chars) #\") (call-with-values (lambda () (read-string (cdr chars)))
                                  (lambda (str rest) (loop rest (cons str tokens)))))
      ((char=? (car chars) #\') (call-with-values (lambda () (read-char-literal (cdr chars)))
                                  (lambda (chr rest) (loop rest (cons chr tokens)))))
      (else (call-with-values (lambda () (read-word chars))
               (lambda (tok rest) (loop rest (cons tok tokens))))))))

(define (drop-until-newline chars)
  (cond ((null? chars) '())
        ((char=? (car chars) #\newline) (cdr chars))
        (else (drop-until-newline (cdr chars)))))

(define (drop-comment chars)
  (cond ((null? chars) '())
        ((and (char=? (car chars) #\*) (not (null? (cdr chars))) (char=? (cadr chars) #\)))
         (cddr chars))
        (else (drop-comment (cdr chars)))))

(define (read-string chars)
  (let loop ((chars chars) (acc '(#\")))
    (cond ((null? chars) (values (list->string (reverse acc)) '()))
          ((char=? (car chars) #\") (values (list->string (reverse (cons #\" acc))) (cdr chars)))
          (else (loop (cdr chars) (cons (car chars) acc))))))

(define (read-char-literal chars)
  (cond ((null? chars) (values "'" '()))
        ((char=? (car chars) #\\)
         (let loop ((chars (cdr chars)) (acc '(#\\ #\')))
           (cond ((null? chars) (values (list->string (reverse acc)) '()))
                 ((char-numeric? (car chars)) (loop (cdr chars) (cons (car chars) acc)))
                 ((member (car chars) '(#\n #\t #\r #\b))
                  (values (list->string (reverse (cons (car chars) acc))) (cdr chars)))
                 (else (values (list->string (reverse (cons (car chars) acc))) (cdr chars))))))
        (else (values (string-append "'" (string (car chars))) (cdr chars)))))

(define (read-word chars)
  (let loop ((chars chars) (acc '()))
    (cond ((null? chars) (values (list->string (reverse acc)) '()))
          ((or (char-whitespace? (car chars))
               (member (car chars) '(#\[ #\] #\. #\( #\# #\")))
           (values (list->string (reverse acc)) chars))
          (else (loop (cdr chars) (cons (car chars) acc))))))

;;; Tokenize and parse a string into Joy expressions
(define (joy-tokenize str)
  (joy-parse-tokens (map joy-parse-token (joy-split (string->list str)))))

;;; Read and parse Joy input from the user or file
(define (joy-read-all)
  (let loop ((chars '()))
    (let ((c (read-char (current-input-port))))
      (if (eof-object? c)
          (joy-tokenize (list->string (reverse chars)))
          (loop (cons c chars))))))

;;; ===========================================================================
;;; MARK: Execution Engine
;;; ===========================================================================

;;; Execute a Joy program (list of tokens)
(define (joy-exec code)
  (let loop ((code code))
    (if (null? code)
        (joy-ok #t)
        (let ((res (joy-exec-one (car code))))
          (if (joy-err? res)
              res
              (loop (cdr code)))))))

;;; Evaluate a single Joy token: resolve symbol, run procedure, or push value
(define (joy-exec-one token)
  (cond
    ((symbol? token)
     (let ((val (joy-get token)))
       (cond
         ((procedure? val) (val))
         ((list? val) (joy-exec val))
         ((null? val) (joy-err (string-append "Undefined symbol: " (symbol->string token))))
         (else (joy-push! val) (joy-ok #t)))))
    (else (joy-push! token) (joy-ok #t))))

;;; Print the current state of the Joy stack
(define (joy-print-stack)
  (display "Stack: ")
  (let ((unified (append (car joy-stack) (reverse (cdr joy-stack)))))
    (for-each (lambda (x) (joy-write x) (display " "))
      (reverse unified)))
  (newline))

;;; ===========================================================================
;;; MARK: Macro Definitions
;;; ===========================================================================

(define-syntax joy-define
  (syntax-rules ()
    ((_ name body ...)
      (joy-set! 'name '(body ...)))))

;;; Define a Joy word that returns a single result
(define-syntax joy-prim
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (let ((res (begin . body)))
              (if (joy-err? res)
                  res
                  (begin (joy-push! res) (joy-ok #t))))))))))

;;; Define a Joy word that performs side effects without returning a value
(define-syntax joy-prim-void
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (begin . body)
            (joy-ok #t)))))))

;;; Define a Joy word that returns multiple values (as list)
(define-syntax joy-prim-list
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (let ((res (begin . body)))
              (if (joy-err? res)
                  res
                  (begin (joy-push-list! res) (joy-ok #t))))))))))

;;; Pattern-match and bind Joy stack arguments to variables
(define-syntax joy-let
  (syntax-rules ()
    ((_ () . body)
      (begin . body))
    ((_ (x . rest) . body)
      (let ((res (joy-pop!)))
        (if (joy-err? res)
            res
            (let ((x (joy-res-val res)))
              (joy-let rest . body)))))))

;;; =============================================================================
;;; MARK: Core Joy Primitives
;;; =============================================================================

;;; Conversion and display
(define (joy-write x)
  (cond ((eq? x #t) (display "true"))
        ((eq? x #f) (display "false"))
        (else (write x))))

;;; Print the top of the stack and remove it (Joy's `.`)
(joy-prim-void (dot x)
  (joy-write x)
  (newline))

(joy-set! '.dot (joy-get 'dot))

;;; Stack inspection
(joy-prim-void (stack) (joy-push! (append (car joy-stack) (reverse (cdr joy-stack)))))
(joy-prim-void (env) (joy-push! joy-env))
(joy-prim-void (peek) (let ((res (joy-peek)))
                       (if (joy-err? res) res (begin (joy-push! (joy-res-val res)) (joy-ok #t)))))
(joy-prim-void (size) (joy-push! (joy-stack-size)))
(joy-prim-void (clear) (joy-clear-stack!))

;;; Stack manipulation
(joy-prim-list (dup x) (list x x))
(joy-prim-void (pop x) #f)
(joy-prim-list (swap x y) (list y x))
(joy-prim-list (rot x y z) (list y z x))
(joy-prim-list (rollup x y z) (list z x y))
(joy-prim-list (rolldown x y z) (list y z x))
(joy-prim-list (over x y) (list x y x))
(joy-prim-list (pick x y z) (list x y z x))
(joy-prim-list (tuck x y) (list y x y))
(joy-prim (nip x y) y)

;;; Dip combinators
(joy-prim-void (dip quot x)
  (if (not (list? quot))
    (joy-error "dip expects a quotation")
    (begin
      (joy-exec quot)
      (joy-push! x))))

(joy-prim-void (dip2 quot x y)
  (if (not (list? quot))
    (joy-error "dip2 expects a quotation")
    (begin
      (joy-exec quot)
      (joy-push! y)
      (joy-push! x))))

;;; Arithmetic
(joy-prim (+ x y)
  (cond ((and (number? x) (number? y)) (+ x y))
        ((and (char? x) (integer? y)) (integer->char (+ (char->integer x) y)))
        ((and (integer? x) (char? y)) (integer->char (+ x (char->integer y))))
        (else (joy-err "type error: + expects numbers or char+int"))))

(joy-prim (- x y)
  (cond ((and (number? x) (number? y)) (- x y))
        ((and (char? x) (integer? y)) (integer->char (- (char->integer x) y)))
        ((and (char? x) (char? y)) (- (char->integer x) (char->integer y)))
        (else (joy-err "type error: - expects numbers or char+int/char"))))
(joy-prim (* x y) (* x y))
(joy-prim (/ x y) (if (zero? y) (joy-err "Division by zero") (/ x y)))
(joy-prim (div x y) (if (zero? y) (joy-err "Division by zero") (quotient x y)))
(joy-prim (mod x y) (if (zero? y) (joy-err "Division by zero") (remainder x y)))
(joy-prim (abs x) (abs x))
(joy-prim (neg x) (- x))
(joy-prim (sqrt x) (sqrt x))
(joy-prim (pow x y) (expt x y))
(joy-prim (succ x) (if (char? x) (integer->char (+ (char->integer x) 1)) (+ x 1)))
(joy-prim (pred x) (if (char? x) (integer->char (- (char->integer x) 1)) (- x 1)))

;;; Comparison
(joy-prim (= x y) (equal? x y))
(joy-prim (equal x y) (equal? x y))
(joy-prim (< x y) (< x y))
(joy-prim (> x y) (> x y))
(joy-prim (<= x y) (<= x y))
(joy-prim (>= x y) (>= x y))
(joy-prim (not= x y) (not (= x y)))

;;; Predicates
(joy-prim (zero? x) (= x 0))
(joy-prim (pos? x) (> x 0))
(joy-prim (neg? x) (< x 0))
(joy-prim (odd? x) (odd? x))
(joy-prim (even? x) (even? x))
(joy-prim (small? x) (and (integer? x) (< x 100)))

;;; Logical
(joy-prim (and x y) (and x y))
(joy-prim (or x y) (or x y))
(joy-prim (not x) (not x))

;;; Type checking
(joy-prim (integer? x) (integer? x))
(joy-prim (boolean? x) (boolean? x))
(joy-prim (list? x) (list? x))
(joy-prim (symbol? x) (symbol? x))
(joy-prim (char? x) (char? x))
(joy-prim (string? x) (string? x))
(joy-prim (null? x) (null? x))

;;; Conversions
(joy-prim (char x)
  (cond ((integer? x) (integer->char x))
        ((char? x) (char->integer x))
        (else (joy-error "char expects integer or character"))))

;;; List operations
(joy-prim (cons x y) (cons x y))
(joy-prim-list (uncons x) (list (car x) (cdr x)))
(joy-prim (first x) (car x))
(joy-prim (rest x) (cdr x))
(joy-prim (length x) (length x))
(joy-prim (append x y) (append x y))
(joy-prim (reverse x) (reverse x))
(joy-prim (second x) (car (cdr x)))
(joy-prim (third x) (car (cdr (cdr x))))

;;; Helper functions for take and drop
(define (take n lst)
  (if (or (zero? n) (null? lst))
    '()
    (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (zero? n) (null? lst))
    lst
    (drop (- n 1) (cdr lst))))

(joy-prim (take n lst) (take n lst))
(joy-prim (drop n lst) (drop n lst))
(joy-prim (at n lst) (list-ref lst n))

;;; I/O
(joy-prim-void (print x) (write x) (newline))
(joy-prim-void (put x) (write x) (display " "))

;;; =============================================================================
;;; MARK: Quotation Combinators
;;; =============================================================================

;;; Execute a quotation on top of the stack
(joy-prim-void (i quot)
  (if (list? quot)
    (joy-exec quot)
    (joy-error "i expects a quotation")))

;;; Conditional execution: run one of two quotations based on test result
(joy-prim-void (ifte test then-branch else-branch)
  (if (not (and (list? then-branch) (list? else-branch)))
    (joy-error "ifte expects two quotations")
    (begin
      (joy-push! test)
      (joy-exec '(i))
      (let ((condition (joy-pop!)))
        (if condition
          (joy-exec then-branch)
          (joy-exec else-branch))))))

;;; Apply a quotation to each item in a list, collecting results
(joy-prim-void (map quot lst)
  (if (not (list? quot))
    (joy-error "map expects a quotation")
    (joy-push! (map (lambda (x)
                      (joy-push! x)
                      (joy-exec quot)
                      (joy-pop!))
                 lst))))

;;; Apply a quotation to filter a list based on truthy result
(joy-prim-void (filter quot lst)
  (if (not (list? quot))
    (joy-error "filter expects a quotation")
    (joy-push! (filter (lambda (x)
                         (joy-push! x)
                         (joy-exec quot)
                         (joy-pop!))
                 lst))))

;;; Reduce a list using a quotation and an initial value
(joy-prim-void (fold quot init lst)
  (if (not (list? quot))
    (joy-error "fold expects a quotation")
    (joy-push! (fold-left (lambda (acc x)
                            (joy-push! acc)
                            (joy-push! x)
                            (joy-exec quot)
                            (joy-pop!))
                 init
                 lst))))

;;; Execute a quotation n times
(joy-prim-void (times quot n)
  (if (not (list? quot))
    (joy-error "times expects a quotation")
    (let loop ((count n))
      (when (> count 0)
        (joy-exec quot)
        (loop (- count 1))))))

;;; Repeatedly execute body while test quotation evaluates true
(joy-prim-void (while test-quot body-quot)
  (if (not (and (list? test-quot) (list? body-quot)))
    (joy-error "while expects two quotations")
    (let loop ()
      (joy-exec test-quot)
      (when (joy-pop!)
        (joy-exec body-quot)
        (loop)))))

;;; Apply a quotation to each item in a list (like for-each)
(joy-prim-void (step quot lst)
  (if (not (list? quot))
    (joy-error "step expects a quotation")
    (for-each (lambda (x)
                (joy-push! x)
                (joy-exec quot))
      lst)))

;;; Execute one of two quotations based on boolean condition
(joy-prim-void (branch condition then-branch else-branch)
  (if (not (and (list? then-branch) (list? else-branch)))
    (joy-error "branch expects two quotations")
    (if condition
      (joy-exec then-branch)
      (joy-exec else-branch))))

;;; Evaluate a quotation in isolation and push its result
(joy-prim-void (nullary quot)
  (if (not (list? quot))
    (joy-error "nullary expects a quotation")
    (let ((saved-f (car joy-stack)) (saved-b (cdr joy-stack)))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set-car! joy-stack saved-f)
        (set-cdr! joy-stack saved-b)
        (joy-push! result)))))

;;; Apply a quotation to a copy of the top stack item and restore original
(joy-prim-void (unary quot x)
  (if (not (list? quot))
    (joy-error "unary expects a quotation")
    (let ((saved-f (car joy-stack)) (saved-b (cdr joy-stack)))
      (joy-clear-stack!)
      (joy-push! x)
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set-car! joy-stack saved-f)
        (set-cdr! joy-stack saved-b)
        (joy-push! x)
        (joy-push! result)))))

;;; Apply a quotation to the top two stack items, restore originals, and push result
(joy-prim-void (binary quot x y)
  (if (not (list? quot))
    (joy-error "binary expects a quotation")
    (let ((saved-f (car joy-stack)) (saved-b (cdr joy-stack)))
      (joy-clear-stack!)
      (joy-push! y)
      (joy-push! x)
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set-car! joy-stack saved-f)
        (set-cdr! joy-stack saved-b)
        (joy-push! y)
        (joy-push! x)
        (joy-push! result)))))

;;; =============================================================================
;;; MARK: Result-type Error Handling
;;; =============================================================================

(joy-define ok [ok] cons)
(joy-define error [error] cons)
(joy-define is-ok? first [ok] =)
(joy-define is-error? first [error] =)
(joy-define unwrap-ok rest first)
(joy-define unwrap-error rest first)

(joy-define safe-div
  [0 =]
  [pop "Division by zero" error]
  [/ ok]
  ifte)

;;; Execute a quotation and wrap result in [ok ...] or [error ...]
(joy-prim-void (try-exec quot)
  (if (not (list? quot))
    (joy-error "try-exec expects a quotation")
    (guard (ex (else (joy-push! `[error ,(condition-message ex)])))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (joy-push! `[ok ,result])))))

(joy-define result-bind
  [is-error?]
  []
  [unwrap-ok swap i]
  ifte)

(joy-define result-map
  [is-error?]
  []
  [unwrap-ok swap i ok]
  ifte)

(joy-define result-chain
  [null?]
  []
  [uncons swap result-bind result-chain]
  ifte)

;;; =============================================================================
;;; MARK: Assorted Joy Definitions
;;; =============================================================================

(joy-define square dup *)
(joy-define cube dup dup * *)
(joy-define twice dup +)
(joy-define factorial [1 =] [pop 1] [dup 1 - factorial *] ifte)
(joy-define sum 0 [+] fold)
(joy-define product 1 [*] fold)
(joy-define max [>] [swap] [] ifte)
(joy-define min [<] [swap] [] ifte)

(joy-define range [0 =] [pop []] [dup 1 - range cons] ifte)

(joy-define safe-sqrt
  [0 <]
  ["Negative square root" error]
  [sqrt ok]
  ifte)

(joy-define parse-int
  [integer?]
  [ok]
  ["Not an integer" error]
  ifte)

;;; =============================================================================
;;; MARK: REPL
;;; =============================================================================

;;; Start the Joy interactive shell (REPL)
(define (joy-repl)
  (let ((line (get-line (current-input-port))))
    (cond
      ((eof-object? line)
        (display "Goodbye.\n"))
      (else
        (guard (ex (else (display "Error: ") (display (condition-message ex)) (newline)))
          (joy-exec (joy-tokenize line))
          (joy-print-stack))
        (joy-repl)))))

(joy-prim-void (help)
  (for-each (lambda (x) (display x) (newline))
    '("Joy Interpreter Commands:"
       "  Stack: dup pop swap rot rollup rolldown size clear"
       "  Arithmetic: + - * / div mod abs neg sqrt pow"
       "  Comparison: = < > <= >= not="
       "  Logical: and or not"
       "  Lists: cons uncons first rest length append reverse"
       "  Quotations: i ifte map filter fold times while"
       "  Types: integer? boolean? list? symbol? null?"
       "  I/O: print put"
       "  Examples: square cube factorial sum product"
       "  Use [ ] for quotations, e.g., [1 2 3] [dup *] map")))

;;; Result words
(joy-define ok [ok] cons)
(joy-define err [err] cons)
(joy-define ok? first [ok] equal)
(joy-define err? first [err] equal)
(joy-define map-ok [ok?] [rest first swap i ok] [] ifte)
(joy-define bind-ok [ok?] [rest first swap i] [] ifte)

;;; Entry point logic
(let ((args (command-line-arguments)))
  (cond
    ((null? args)
     (display "Joy Interpreter. Type 'help' for commands.\n")
     (joy-repl))
    (else
     (joy-exec (joy-read-all))
     (joy-print-stack))))
  (hashtable-set! joy-env sym val))

(define (joy-push! x)
  (set! joy-stack (cons x joy-stack)))

(define (joy-pop!)
  (if (null? joy-stack)
    (joy-error "Stack underflow")
    (let ((top (car joy-stack)))
      (set! joy-stack (cdr joy-stack))
      top)))

(define (joy-peek)
  (if (null? joy-stack)
    (joy-error "Stack underflow on peek")
    (car joy-stack)))

(define (joy-stack-size)
  (length joy-stack))

(define (joy-clear-stack!)
  (set! joy-stack '()))

(define (joy-push-list! lst)
  (for-each joy-push! (reverse lst)))

;;; ===========================================================================
;;; MARK: Tokenization and Parsing
;;; ===========================================================================

;;; Convert a raw token string into a Scheme object:
;;; number, boolean, symbol, or quotation bracket
(define (joy-parse-token tok)
  (cond
    ((string=? tok "[") 'open-bracket)
    ((string=? tok "]") 'close-bracket)
    ((string=? tok ".") '.dot)
    ((string->number tok) => values)
    ((string=? tok "true") #t)
    ((string=? tok "false") #f)
    (else (string->symbol tok))))

;;; Convert a flat token list into a Joy expression list,
;;; properly grouping quotations with brackets
(define (joy-parse-tokens tokens)
  (let loop ((tokens tokens) (result '()) (quote-stack '()))
    (cond
      ((null? tokens)
        (if (null? quote-stack)
          (reverse result)
          (joy-error "Unmatched opening bracket")))
      ((eq? (car tokens) 'open-bracket)
        (loop (cdr tokens) result (cons '() quote-stack)))
      ((eq? (car tokens) 'close-bracket)
        (if (null? quote-stack)
          (joy-error "Unmatched closing bracket")
          (let ((current-quote (car quote-stack))
                (remaining-stack (cdr quote-stack)))
            (if (null? remaining-stack)
              (loop (cdr tokens)
                (cons (reverse current-quote) result)
                '())
              (loop (cdr tokens)
                result
                (cons (cons (reverse current-quote)
                        (car remaining-stack))
                  (cdr remaining-stack)))))))
      (else
        (if (null? quote-stack)
          (loop (cdr tokens)
            (cons (car tokens) result)
            quote-stack)
          (loop (cdr tokens)
            result
            (cons (cons (car tokens) (car quote-stack))
              (cdr quote-stack))))))))

;;; Split a character list into token strings.
;;; Treats whitespace as separator and brackets as distinct tokens.
(define (joy-split chars)
  (define (whitespace? c) (member c '(#\space #\tab #\newline)))
  (define (delimiter? c) (member c '(#\[ #\])))
  (define (next-token acc chars)
    (cond
      ((null? chars) 
        (if (null? acc) '() (list (list->string (reverse acc)))))
      ((whitespace? (car chars))
        (if (null? acc)
          (next-token '() (cdr chars))
          (cons (list->string (reverse acc)) (next-token '() (cdr chars)))))
      ((delimiter? (car chars))
        (if (null? acc)
          (cons (string (car chars)) (next-token '() (cdr chars)))
          (cons (list->string (reverse acc))
            (cons (string (car chars)) (next-token '() (cdr chars))))))
      (else
        (next-token (cons (car chars) acc) (cdr chars)))))
  (next-token '() chars))

;;; Tokenize and parse a string into Joy expressions
(define (joy-tokenize str)
  (joy-parse-tokens (map joy-parse-token (joy-split (string->list str)))))

;;; Read and parse one line of Joy input from the user
(define (joy-read-line)
  (let ((line (get-line (current-input-port))))
    (if (eof-object? line)
      line
      (joy-tokenize line))))

;;; ===========================================================================
;;; MARK: Execution Engine
;;; ===========================================================================

;;; Execute a Joy program (list of tokens)
(define (joy-exec code)
  (for-each joy-exec-one code))

;;; Evaluate a single Joy token: resolve symbol, run procedure, or push value
(define (joy-exec-one token)
  (cond
    ((symbol? token)
      (let ((val (joy-get token)))
        (cond
          ((procedure? val) (val))
          ((list? val) (joy-exec val))
          ((null? val) (joy-error "Undefined symbol: " (symbol->string token)))
          (else (joy-push! val)))))
    (else (joy-push! token))))

;;; Print the current state of the Joy stack
(define (joy-print-stack)
  (display "Stack: ")
  (for-each (lambda (x)
              (write x)
              (display " "))
    (reverse joy-stack))
  (newline))

;;; ===========================================================================
;;; MARK: Macro Definitions
;;; ===========================================================================

(define-syntax joy-define
  (syntax-rules ()
    ((_ name body ...)
      (joy-set! 'name '(body ...)))))

;;; Define a Joy word that returns a single result
(define-syntax joy-prim
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (joy-push! (begin . body))))))))

;;; Define a Joy word that performs side effects without returning a value
(define-syntax joy-prim-void
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (begin . body)))))))

;;; Define a Joy word that returns multiple values (as list)
(define-syntax joy-prim-list
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-let args
            (joy-push-list! (begin . body))))))))

;;; Pattern-match and bind Joy stack arguments to variables
(define-syntax joy-let
  (syntax-rules ()
    ((_ () . body)
      (begin . body))
    ((_ (x . rest) . body)
      (let ((x (joy-pop!)))
        (joy-let rest . body)))))

;;; =============================================================================
;;; MARK: Core Joy Primitives
;;; =============================================================================

;;; Print the top of the stack and remove it (Joy's `.`)
(joy-prim-void (dot x)
  (write x)
  (newline))

(joy-set! '.dot (joy-get 'dot))


;;; Stack inspection
(joy-prim-void (stack) (joy-push! joy-stack))
(joy-prim-void (env) (joy-push! joy-env))
(joy-prim-void (peek) (joy-push! (joy-peek)))
(joy-prim-void (size) (joy-push! (joy-stack-size)))
(joy-prim-void (clear) (joy-clear-stack!))

;;; Stack manipulation
(joy-prim-list (dup x) (list x x))
(joy-prim-void (pop x) #f)
(joy-prim-list (swap x y) (list y x))
(joy-prim-list (rot x y z) (list y z x))
(joy-prim-list (rollup x y z) (list z x y))
(joy-prim-list (rolldown x y z) (list y z x))
(joy-prim-list (over x y) (list x y x))
(joy-prim-list (pick x y z) (list x y z x))
(joy-prim-list (tuck x y) (list y x y))
(joy-prim (nip x y) y)

;;; Dip combinators
(joy-prim-void (dip quot x)
  (if (not (list? quot))
    (joy-error "dip expects a quotation")
    (begin
      (joy-exec quot)
      (joy-push! x))))

(joy-prim-void (dip2 quot x y)
  (if (not (list? quot))
    (joy-error "dip2 expects a quotation")
    (begin
      (joy-exec quot)
      (joy-push! y)
      (joy-push! x))))

;;; Arithmetic
(joy-prim (+ x y) (+ x y))
(joy-prim (- x y) (- x y))
(joy-prim (* x y) (* x y))
(joy-prim (/ x y) (/ x y))
(joy-prim (div x y) (quotient x y))
(joy-prim (mod x y) (remainder x y))
(joy-prim (abs x) (abs x))
(joy-prim (neg x) (- x))
(joy-prim (sqrt x) (sqrt x))
(joy-prim (pow x y) (expt x y))

;;; Comparison
(joy-prim (= x y) (= x y))
(joy-prim (< x y) (< x y))
(joy-prim (> x y) (> x y))
(joy-prim (<= x y) (<= x y))
(joy-prim (>= x y) (>= x y))
(joy-prim (not= x y) (not (= x y)))

;;; Predicates
(joy-prim (zero? x) (= x 0))
(joy-prim (pos? x) (> x 0))
(joy-prim (neg? x) (< x 0))
(joy-prim (odd? x) (odd? x))
(joy-prim (even? x) (even? x))
(joy-prim (small? x) (and (integer? x) (< x 100)))

;;; Logical
(joy-prim (and x y) (and x y))
(joy-prim (or x y) (or x y))
(joy-prim (not x) (not x))

;;; Type checking
(joy-prim (integer? x) (integer? x))
(joy-prim (boolean? x) (boolean? x))
(joy-prim (list? x) (list? x))
(joy-prim (symbol? x) (symbol? x))
(joy-prim (null? x) (null? x))

;;; List operations
(joy-prim (cons x y) (cons x y))
(joy-prim-list (uncons x) (list (car x) (cdr x)))
(joy-prim (first x) (car x))
(joy-prim (rest x) (cdr x))
(joy-prim (length x) (length x))
(joy-prim (append x y) (append x y))
(joy-prim (reverse x) (reverse x))
(joy-prim (second x) (car (cdr x)))
(joy-prim (third x) (car (cdr (cdr x))))

;;; Helper functions for take and drop
(define (take n lst)
  (if (or (zero? n) (null? lst))
    '()
    (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (zero? n) (null? lst))
    lst
    (drop (- n 1) (cdr lst))))

(joy-prim (take n lst) (take n lst))
(joy-prim (drop n lst) (drop n lst))
(joy-prim (at n lst) (list-ref lst n))

;;; I/O
(joy-prim-void (print x) (write x) (newline))
(joy-prim-void (put x) (write x) (display " "))

;;; =============================================================================
;;; MARK: Quotation Combinators
;;; =============================================================================

;;; Execute a quotation on top of the stack
(joy-prim-void (i quot)
  (if (list? quot)
    (joy-exec quot)
    (joy-error "i expects a quotation")))

;;; Conditional execution: run one of two quotations based on test result
(joy-prim-void (ifte test then-branch else-branch)
  (if (not (and (list? then-branch) (list? else-branch)))
    (joy-error "ifte expects two quotations")
    (begin
      (joy-push! test)
      (joy-exec '(i))
      (let ((condition (joy-pop!)))
        (if condition
          (joy-exec then-branch)
          (joy-exec else-branch))))))

;;; Apply a quotation to each item in a list, collecting results
(joy-prim-void (map quot lst)
  (if (not (list? quot))
    (joy-error "map expects a quotation")
    (joy-push! (map (lambda (x)
                      (joy-push! x)
                      (joy-exec quot)
                      (joy-pop!))
                 lst))))

;;; Apply a quotation to filter a list based on truthy result
(joy-prim-void (filter quot lst)
  (if (not (list? quot))
    (joy-error "filter expects a quotation")
    (joy-push! (filter (lambda (x)
                         (joy-push! x)
                         (joy-exec quot)
                         (joy-pop!))
                 lst))))

;;; Reduce a list using a quotation and an initial value
(joy-prim-void (fold quot init lst)
  (if (not (list? quot))
    (joy-error "fold expects a quotation")
    (joy-push! (fold-left (lambda (acc x)
                            (joy-push! acc)
                            (joy-push! x)
                            (joy-exec quot)
                            (joy-pop!))
                 init
                 lst))))

;;; Execute a quotation n times
(joy-prim-void (times quot n)
  (if (not (list? quot))
    (joy-error "times expects a quotation")
    (let loop ((count n))
      (when (> count 0)
        (joy-exec quot)
        (loop (- count 1))))))

;;; Repeatedly execute body while test quotation evaluates true
(joy-prim-void (while test-quot body-quot)
  (if (not (and (list? test-quot) (list? body-quot)))
    (joy-error "while expects two quotations")
    (let loop ()
      (joy-exec test-quot)
      (when (joy-pop!)
        (joy-exec body-quot)
        (loop)))))

;;; Apply a quotation to each item in a list (like for-each)
(joy-prim-void (step quot lst)
  (if (not (list? quot))
    (joy-error "step expects a quotation")
    (for-each (lambda (x)
                (joy-push! x)
                (joy-exec quot))
      lst)))

;;; Execute one of two quotations based on boolean condition
(joy-prim-void (branch condition then-branch else-branch)
  (if (not (and (list? then-branch) (list? else-branch)))
    (joy-error "branch expects two quotations")
    (if condition
      (joy-exec then-branch)
      (joy-exec else-branch))))

;;; Evaluate a quotation in isolation and push its result
(joy-prim-void (nullary quot)
  (if (not (list? quot))
    (joy-error "nullary expects a quotation")
    (let ((saved-stack joy-stack))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set! joy-stack saved-stack)
        (joy-push! result)))))

;;; Apply a quotation to a copy of the top stack item and restore original
(joy-prim-void (unary quot x)
  (if (not (list? quot))
    (joy-error "unary expects a quotation")
    (let ((saved-stack joy-stack))
      (set! joy-stack (list x))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set! joy-stack saved-stack)
        (joy-push! x)
        (joy-push! result)))))

;;; Apply a quotation to the top two stack items, restore originals, and push result
(joy-prim-void (binary quot x y)
  (if (not (list? quot))
    (joy-error "binary expects a quotation")
    (let ((saved-stack joy-stack))
      (set! joy-stack (list y x))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (set! joy-stack saved-stack)
        (joy-push! y)
        (joy-push! x)
        (joy-push! result)))))

;;; =============================================================================
;;; MARK: Result-type Error Handling
;;; =============================================================================

(joy-define ok [ok] cons)
(joy-define error [error] cons)
(joy-define is-ok? first [ok] =)
(joy-define is-error? first [error] =)
(joy-define unwrap-ok rest first)
(joy-define unwrap-error rest first)

(joy-define safe-div
  [0 =]
  [pop "Division by zero" error]
  [/ ok]
  ifte)

;;; Execute a quotation and wrap result in [ok ...] or [error ...]
(joy-prim-void (try-exec quot)
  (if (not (list? quot))
    (joy-error "try-exec expects a quotation")
    (guard (ex (else (joy-push! `[error ,(condition-message ex)])))
      (joy-exec quot)
      (let ((result (joy-pop!)))
        (joy-push! `[ok ,result])))))

(joy-define result-bind
  [is-error?]
  []
  [unwrap-ok swap i]
  ifte)

(joy-define result-map
  [is-error?]
  []
  [unwrap-ok swap i ok]
  ifte)

(joy-define result-chain
  [null?]
  []
  [uncons swap result-bind result-chain]
  ifte)

;;; =============================================================================
;;; MARK: Assorted Joy Definitions
;;; =============================================================================

(joy-define square dup *)
(joy-define cube dup dup * *)
(joy-define twice dup +)
(joy-define factorial [1 =] [pop 1] [dup 1 - factorial *] ifte)
(joy-define sum 0 [+] fold)
(joy-define product 1 [*] fold)
(joy-define max [>] [swap] [] ifte)
(joy-define min [<] [swap] [] ifte)

(joy-define range [0 =] [pop []] [dup 1 - range cons] ifte)

(joy-define safe-sqrt
  [0 <]
  ["Negative square root" error]
  [sqrt ok]
  ifte)

(joy-define parse-int
  [integer?]
  [ok]
  ["Not an integer" error]
  ifte)

;;; =============================================================================
;;; MARK: REPL
;;; =============================================================================

;;; Start the Joy interactive shell (REPL)
(define (joy-repl)
  (let ((expr (joy-read-line)))
    (cond
      ((eof-object? expr)
        (display "Goodbye.\n"))
      (else
        (guard (ex (else (display "Error: ") (display (condition-message ex)) (newline)))
          (joy-exec expr)
          (joy-print-stack))
        (joy-repl)))))

(joy-prim-void (help)
  (for-each (lambda (x) (display x) (newline))
    '("Joy Interpreter Commands:"
       "  Stack: dup pop swap rot rollup rolldown size clear"
       "  Arithmetic: + - * / div mod abs neg sqrt pow"
       "  Comparison: = < > <= >= not="
       "  Logical: and or not"
       "  Lists: cons uncons first rest length append reverse"
       "  Quotations: i ifte map filter fold times while"
       "  Types: integer? boolean? list? symbol? null?"
       "  I/O: print put"
       "  Examples: square cube factorial sum product"
       "  Use [ ] for quotations, e.g., [1 2 3] [dup *] map")))

;;; Entry point logic
(let ((args (command-line-arguments)))
  (cond
    ((null? args)
     ;; Default to interactive REPL if stdin is a TTY (roughly)
     ;; For simplicity, we just check if there's any input to read
     (display "Joy Interpreter. Type 'help' for commands.\n")
     (joy-repl))
    (else
     ;; In script mode, we could load files, but for now just run
     ;; This part can be expanded later
     (joy-repl))))

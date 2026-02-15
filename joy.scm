(import (rnrs)
        (rnrs hashtables)
        (only (chezscheme) format))

(display "Starting Joy interpreter...\n")

;;; =============================================================================
;;; MARK: Core Joy Environment
;;; ========Global symbol environment and data stack for Joy programs============

;;; Global symbol environment and data stack for Joy programs
(define joy-env (make-eq-hashtable))
(define joy-stack '())

;;; Raise a Joy-specific error with a message
(define (joy-error . args)
  (error 'joy (apply string-append args)))

;;; Convert a Scheme object into its string representation
(define (write-to-string obj)
  (with-output-to-string
    (lambda ()
      (write obj))))

;;; Fold left over a list using a binary function
(define (fold-left f init lst)
  (if (null? lst)
    init
    (fold-left f (f init (car lst)) (cdr lst))))

;;; =============================================================================
;;; MARK: Environment and Stack Operations
;;; =============================================================================

(define (joy-get sym)
  (hashtable-ref joy-env sym '()))

(define (joy-set! sym val)
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

;;; =============================================================================
;;; MARK: Tokenization and Parsing
;;; =============================================================================

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
  (display "> ")
  (flush-output-port (current-output-port))
  (let ((line (get-line (current-input-port))))
    (if (eof-object? line)
      line
      (joy-tokenize line))))

;;; =============================================================================
;;; MARK: Execution Engine
;;; =============================================================================

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

;;; =============================================================================
;;; MARK: Macro Definitions
;;; =============================================================================

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
  (display "> ")
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

;;; Start the REPL
(display "Type 'help' for available commands.\n")
(joy-repl)

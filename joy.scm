;;; joy.scm -- Joy interpreter for Chez Scheme.
;;;
;;; Implements Manfred von Thun's purely functional concatenative language:
;;; - Data stack is represented as a double-ended queue (deque).
;;; - Anonymous logic variables sit alongside standard Joy words.
;;; - All errors inside Joy code produce tagged joy-result records.

(import (rnrs)
  (rnrs mutable-pairs)
  (only (chezscheme)
    command-line-arguments
    display-condition
    quotient
    remainder))

;;; ---- Machine state ----

(define joy-env (make-eq-hashtable))

;; Data stack as a pair (front . back).
;; FRONT holds items at the top of the stack, top item first.
;; BACK holds items at the bottom of the stack, bottom item first.
;; The whole stack viewed from top to bottom is:
;;   (append front (reverse back))
;; Both push/pop and unshift/shift run in amortized O(1) time.
;; When one half is exhausted, the other half is reversed over.
(define joy-stack (cons '() '()))
(define joy-mode 'stack)

;; Logic variables bound so far, recorded newest-first for rollback.
(define joy-trail '())

;;; ---- Results ----
;; Anything that can fail inside Joy execution returns a joy-result record
;; instead of raising an unhandled Scheme exception.

(define-record-type (joy-result make-joy-result joy-result?)
  (fields (immutable type joy-result-type)
    (immutable value joy-result-value)))

(define (joy-ok val) (make-joy-result 'ok val))
(define (joy-fail) (make-joy-result 'fail #f))
(define (joy-err msg) (make-joy-result 'err msg))
;; The input ended in the middle of a token or form;
;; more lines can complete it.
(define (joy-incomplete why) (make-joy-result 'incomplete why))

(define (joy-result-is? type res)
  (and (joy-result? res) (eq? (joy-result-type res) type)))
(define (joy-ok? res) (joy-result-is? 'ok res))
(define (joy-fail? res) (joy-result-is? 'fail res))
(define (joy-err? res) (joy-result-is? 'err res))
(define (joy-incomplete? res) (joy-result-is? 'incomplete res))
(define (joy-res-val res) (joy-result-value res))

(define (joy-wrap x)
  (if (joy-result? x) x (joy-ok x)))

(define (unspecified? x) (eq? x (if #f #f)))

;;; ---- Macros ----

;; Runs expressions in sequence, stopping at the first non-ok result.
(define-syntax joy-seq
  (syntax-rules ()
    ((_ expr) expr)
    ((_ expr next ...)
      (let ((res expr))
        (if (joy-ok? res)
          (joy-seq next ...)
          res)))))

;; Binds VAR to the inner value of an ok result; non-ok results bubble up.
(define-syntax joy-bind
  (syntax-rules ()
    ((_ (var expr) body ...)
      (let ((res expr))
        (if (joy-ok? res)
          (let ((var (joy-res-val res)))
            body ...)
          res)))))

;; (joy-define name word ...) defines NAME as the Joy program `word ...`.
(define-syntax joy-define
  (syntax-rules ()
    ((_ name body ...)
      (joy-set! 'name '(body ...)))))

;; Pops stack items into the given names in order, top first.
(define-syntax joy-let
  (syntax-rules ()
    ((_ () . body)
      (begin . body))
    ((_ (x . rest) . body)
      (joy-bind (x (joy-pop!))
        (joy-let rest . body)))))

;; Defines a primitive that evaluates its body and pushes the result.
;; If the body returns unspecified void, nothing is pushed.
(define-syntax joy-prim
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-bind (res (joy-wrap (joy-let args (begin . body))))
            (cond ((unspecified? res) (joy-ok #t))
              (else (begin (joy-push! res) (joy-ok #t))))))))))

;; Defines a primitive that pushes nothing automatically;
;; the body handles all its own stack work directly.
(define-syntax joy-prim-void
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-seq (joy-wrap (joy-let args (begin . body)))
            (joy-ok #t)))))))

;; Defines a primitive whose body returns a list of items;
;; the first element ends up topmost on the stack.
(define-syntax joy-prim-list
  (syntax-rules ()
    ((_ (name . args) . body)
      (joy-set! 'name
        (lambda ()
          (joy-bind (res (joy-wrap (joy-let args (begin . body))))
            (begin (joy-push-list! res) (joy-ok #t))))))))

;;; ---- Environment ----

(define joy-undefined (list 'undefined))

;; Normalizes word names to lowercase symbols for case-insensitive lookup.
(define (joy-name sym)
  (string->symbol (string-downcase (symbol->string sym))))

(define (joy-get sym)
  (hashtable-ref joy-env (joy-name sym) joy-undefined))

(define (joy-set! sym val)
  (hashtable-set! joy-env (joy-name sym) val))

(define (joy-alias! new old)
  (joy-set! new (joy-get old)))

;;; ---- Stack ----

(define (joy-push! x)
  (set-car! joy-stack (cons x (car joy-stack))))

;; When FRONT is empty, reverse BACK to become the new FRONT.
(define (joy-refill-front!)
  (set-car! joy-stack (reverse (cdr joy-stack)))
  (set-cdr! joy-stack '()))

;; When BACK is empty, reverse FRONT to become the new BACK.
(define (joy-refill-back!)
  (set-cdr! joy-stack (reverse (car joy-stack)))
  (set-car! joy-stack '()))

(define (joy-pop!)
  (cond
    ((pair? (car joy-stack))
      (let ((top (caar joy-stack)))
        (set-car! joy-stack (cdar joy-stack))
        (joy-ok top)))
    ((pair? (cdr joy-stack))
      (joy-refill-front!)
      (joy-pop!))
    (else (joy-err "Stack underflow"))))

(define (joy-peek)
  (cond
    ((pair? (car joy-stack)) (joy-ok (caar joy-stack)))
    ((pair? (cdr joy-stack))
      (joy-refill-front!)
      (joy-peek))
    (else (joy-err "Stack underflow on peek"))))

;; Returns all items on the stack as a list, top element first.
(define (joy-stack->list)
  (append (car joy-stack) (reverse (cdr joy-stack))))

(define (joy-stack-size)
  (+ (length (car joy-stack)) (length (cdr joy-stack))))

(define (joy-clear-stack!)
  (set-car! joy-stack '())
  (set-cdr! joy-stack '()))

;; Pushes a list of items such that the first item ends up on top.
(define (joy-push-list! lst)
  (for-each joy-push! (reverse lst)))

;; Captures the current stack state as a pair of lists.
(define (joy-snapshot)
  (cons (car joy-stack) (cdr joy-stack)))

;; Restores the stack from a previous snapshot.
(define (joy-restore! snapshot)
  (set-car! joy-stack (car snapshot))
  (set-cdr! joy-stack (cdr snapshot)))

;; Pushes an item to the bottom of the deque.
(define (joy-unshift! x)
  (set-cdr! joy-stack (cons x (cdr joy-stack))))

;; Removes and returns the item at the bottom of the deque.
(define (joy-shift!)
  (cond
    ((pair? (cdr joy-stack))
      (let ((bottom (cadr joy-stack)))
        (set-cdr! joy-stack (cddr joy-stack))
        (joy-ok bottom)))
    ((pair? (car joy-stack))
      (joy-refill-back!)
      (joy-shift!))
    (else (joy-err "Deque underflow"))))

;;; ---- Logic variables ----

(define joy-unbound (list 'unbound))

(define-record-type (joy-var make-joy-var joy-var?)
  (fields (mutable ref joy-var-ref joy-var-set-ref!)))

(define (joy-free? x)
  (and (joy-var? x) (eq? (joy-var-ref x) joy-unbound)))

;; Follows bound variables until an unbound variable
;; or a concrete ground term is reached.
(define (joy-walk u)
  (if (and (joy-var? u) (not (joy-free? u)))
    (joy-walk (joy-var-ref u))
    u))

;; Binds variable U to value V and records U on the rollback trail.
(define (joy-bind-var! u v)
  (joy-var-set-ref! u v)
  (set! joy-trail (cons u joy-trail)))

;; Unbinds all variables recorded on the trail since the OLD-TRAIL snapshot.
(define (joy-rollback! old-trail)
  (let loop ()
    (unless (eq? joy-trail old-trail)
      (joy-var-set-ref! (car joy-trail) joy-unbound)
      (set! joy-trail (cdr joy-trail))
      (loop))))

;; Core structural unification between terms U and V.
(define (joy-unify u v)
  (let ((u (joy-walk u)) (v (joy-walk v)))
    (cond
      ((eq? u v) #t)
      ((joy-free? u) (joy-bind-var! u v) #t)
      ((joy-free? v) (joy-bind-var! v u) #t)
      ((and (pair? u) (pair? v))
        (and (joy-unify (car u) (car v))
          (joy-unify (cdr u) (cdr v))))
      (else (equal? u v)))))

;; Transactional unification: if unification fails, all partial bindings
;; created during the attempt are cleanly rolled back.
(define (joy-try-unify u v)
  (let ((mark joy-trail))
    (or (joy-unify u v)
      (begin
        (joy-rollback! mark)
        #f))))

;;; ---- Lexer ----
;; Delimiters (`[`, `]`, `{`, `}`, `.`, `;`) are mapped to internal symbols
;; that cannot clash with user-defined words.

(define tok-open-list (string->symbol "["))
(define tok-close-list (string->symbol "]"))
(define tok-open-set (string->symbol "{"))
(define tok-close-set (string->symbol "}"))
(define tok-end (string->symbol "."))
(define tok-sep (string->symbol ";"))

(define (comment-start? chars)
  (and (char=? (car chars) #\()
    (pair? (cdr chars))
    (char=? (cadr chars) #\*)))

(define (drop-until-newline chars)
  (cond ((null? chars) '())
    ((char=? (car chars) #\newline) (cdr chars))
    (else (drop-until-newline (cdr chars)))))

;; Skips input until the closing `*)`. Returns remaining characters,
;; or #f if the comment is never terminated.
(define (drop-comment chars)
  (cond ((null? chars) #f)
    ((and (char=? (car chars) #\*)
       (pair? (cdr chars))
       (char=? (cadr chars) #\)))
      (cddr chars))
    (else (drop-comment (cdr chars)))))

;; Reads a string literal after the opening double quote.
;; Returns the full quoted literal string and remaining characters,
;; or (#f, '()) if the terminating quote is missing.
(define (read-string-literal chars)
  (let loop ((chars chars) (acc '(#\")))
    (cond ((null? chars) (values #f '()))
      ((char=? (car chars) #\")
        (values (list->string (reverse (cons #\" acc))) (cdr chars)))
      (else (loop (cdr chars) (cons (car chars) acc))))))

;; Reads a character literal after the opening single quote.
;; Handles escape sequences like \n, \t, and octal/decimal numeric escapes.
(define (read-char-literal chars)
  (cond ((null? chars) (values "'" '()))
    ((char=? (car chars) #\\)
      (let loop ((chars (cdr chars)) (acc '(#\\ #\')))
        (cond ((null? chars)
                (values (list->string (reverse acc)) '()))
          ((char-numeric? (car chars))
            (loop (cdr chars) (cons (car chars) acc)))
          ((member (car chars) '(#\n #\t #\r #\b))
            (values (list->string (reverse (cons (car chars) acc)))
              (cdr chars)))
          (else (values (list->string (reverse acc)) chars)))))
    (else (values (string-append "'" (string (car chars))) (cdr chars)))))

;; Distinguishes decimal floating points like 3.14 from word termination dots.
(define (decimal-point? chars acc)
  (and (char=? (car chars) #\.)
    (char-numeric? (car acc))
    (pair? (cdr chars))
    (char-numeric? (cadr chars))))

(define (word-ends-at? chars)
  (or (char-whitespace? (car chars))
    (memv (car chars) '(#\[ #\] #\{ #\} #\. #\# #\" #\;))
    (comment-start? chars)))

;; Reads an identifier or number token. Always consumes at least one character.
(define (read-word chars)
  (let loop ((chars (cdr chars)) (acc (list (car chars))))
    (if (or (null? chars)
          (and (not (decimal-point? chars acc)) (word-ends-at? chars)))
      (values (list->string (reverse acc)) chars)
      (loop (cdr chars) (cons (car chars) acc)))))

;; Splits source text into token strings.
;; Returns a list of string tokens, or a joy-incomplete result if a string
;; or comment was left unclosed at EOF.
(define (joy-split chars)
  (let loop ((chars chars) (tokens '()))
    (if (null? chars)
      (reverse tokens)
      (let ((c (car chars)) (more (cdr chars)))
        (cond
          ((char-whitespace? c) (loop more tokens))
          ((char=? c #\#) (loop (drop-until-newline more) tokens))
          ((comment-start? chars)
            (let ((after (drop-comment (cdr more))))
              (if after
                (loop after tokens)
                (joy-incomplete "Unterminated comment"))))
          ((memv c '(#\[ #\] #\{ #\} #\. #\;))
            (loop more (cons (string c) tokens)))
          ((char=? c #\")
            (call-with-values (lambda () (read-string-literal more))
              (lambda (text rest)
                (if text
                  (loop rest (cons text tokens))
                  (joy-incomplete "Unterminated string")))))
          ((char=? c #\')
            (call-with-values (lambda () (read-char-literal more))
              (lambda (text rest) (loop rest (cons text tokens)))))
          (else
            (call-with-values (lambda () (read-word chars))
              (lambda (text rest) (loop rest (cons text tokens))))))))))

;;; ---- Parser ----

;; Converts a character literal token into a Scheme character.
;; Handles escapes: \n, \t, \r, \b, and decimal numeric codes \ddd.
(define (joy-parse-char tok)
  (let* ((content (substring tok 1 (string-length tok)))
         (len (string-length content)))
    (cond
      ((string=? content "\\n") #\newline)
      ((string=? content "\\t") #\tab)
      ((string=? content "\\r") #\return)
      ((string=? content "\\b") #\backspace)
      ((and (> len 1)
         (char=? (string-ref content 0) #\\)
         (char-numeric? (string-ref content 1)))
        (let ((code (string->number (substring content 1 len) 10)))
          (if (and code (<= 0 code 255))
            (integer->char code)
            (string->symbol (string-downcase tok)))))
      ((= len 1) (string-ref content 0))
      (else (string->symbol (string-downcase tok))))))

;; Parses an individual string token into its typed Joy value representation
;; (number, boolean, character, string, symbol, or structural bracket symbol).
(define (joy-parse-token tok)
  (let ((len (string-length tok)))
    (cond
      ((string=? tok "[") tok-open-list)
      ((string=? tok "]") tok-close-list)
      ((string=? tok "{") tok-open-set)
      ((string=? tok "}") tok-close-set)
      ((string=? tok ".") tok-end)
      ((string=? tok ";") tok-sep)
      ((and (> len 1) (char=? (string-ref tok 0) #\'))
        (joy-parse-char tok))
      ((and (> len 1) (char=? (string-ref tok 0) #\"))
        (substring tok 1 (- len 1)))
      (else
        (let ((n (string->number tok))
              (low (string-downcase tok)))
          (cond
            (n n)
            ((string=? low "true") #t)
            ((string=? low "false") #f)
            (else (string->symbol low))))))))

;; Converts a list of set members into an integer bitmask.
;; Members must be non-negative exact integers or character values.
(define (joy-members->set members)
  (let loop ((members members) (mask 0))
    (if (null? members)
      mask
      (let* ((m (car members))
             (n (if (char? m) (char->integer m) m)))
        (if (and (integer? n) (exact? n) (>= n 0))
          (loop (cdr members)
            (bitwise-ior mask (bitwise-arithmetic-shift-left 1 n)))
          (joy-err
            "Set members must be non-negative integers or characters"))))))

;; Parses a flat stream of tokens into nested Joy quotations and set bitmasks.
;; OPEN maintains the stack of unclosed groups as (kind . reversed-items).
(define (joy-parse-tokens tokens)
  (let loop ((tokens tokens) (done '()) (open '()))
    (define (continue item open)
      (if (null? open)
        (loop (cdr tokens) (cons item done) open)
        (loop (cdr tokens)
          done
          (cons (cons (caar open) (cons item (cdar open))) (cdr open)))))
    (cond
      ((null? tokens)
        (if (null? open)
          (reverse done)
          (joy-incomplete "Unmatched opening bracket")))
      ((eq? (car tokens) tok-open-list)
        (loop (cdr tokens) done (cons (cons 'list '()) open)))
      ((eq? (car tokens) tok-open-set)
        (loop (cdr tokens) done (cons (cons 'set '()) open)))
      ((or (eq? (car tokens) tok-close-list)
           (eq? (car tokens) tok-close-set))
        (cond
          ((null? open)
            (joy-err "Unmatched closing bracket"))
          ((not (eq? (caar open)
                     (if (eq? (car tokens) tok-close-list) 'list 'set)))
            (joy-err "Mismatched brackets"))
          (else
            (let ((value (if (eq? (caar open) 'list)
                           (reverse (cdar open))
                           (joy-members->set (cdar open)))))
              (if (joy-err? value)
                value
                (continue value (cdr open)))))))
      (else (continue (car tokens) open)))))

;; Checks whether a top-level DEFINE declaration
;; has not yet reached its closing dot (`.`).
(define (joy-define-open? items)
  (let scan ((items items) (open? #f))
    (cond
      ((null? items) open?)
      ((eq? (car items) 'define) (scan (cdr items) #t))
      ((eq? (car items) tok-end) (scan (cdr items) #f))
      (else (scan (cdr items) open?)))))

;; Parses raw text into a program list, joy-err, or joy-incomplete.
(define (joy-tokenize text)
  (let ((split (joy-split (string->list text))))
    (if (joy-result? split)
      split
      (let ((program (joy-parse-tokens (map joy-parse-token split))))
        (if (and (list? program) (joy-define-open? program))
          (joy-incomplete "Missing . in DEFINE")
          program)))))

;;; ---- Evaluator ----

;; Parses and stores one or more word definitions following DEFINE:
;;   name == body ; next-name == next-body .
;; Returns the remaining unconsumed tokens following the closing dot.
(define (joy-define-all tokens)
  (if (or (null? tokens) (null? (cdr tokens)))
    (joy-err "Missing name in DEFINE")
    (let ((name (car tokens))
          (after-name (if (eq? (cadr tokens) '==) (cddr tokens) (cdr tokens))))
      (if (not (symbol? name))
        (joy-err "DEFINE expects a name")
        (let find-end ((in after-name) (body '()))
          (cond
            ((null? in)
              (joy-err "Missing . in DEFINE"))
            ((or (eq? (car in) tok-end) (eq? (car in) tok-sep))
              (joy-set! name (reverse body))
              (if (eq? (car in) tok-sep)
                (joy-define-all (cdr in))
                (joy-ok (cdr in))))
            (else
              (find-end (cdr in) (cons (car in) body)))))))))

;; Executes a list of Joy instructions sequentially.
;; Execution terminates early upon the first error result.
(define (joy-exec code)
  (if (not (list? code))
    (if (joy-result? code) code (joy-err "Expected a quotation"))
    (let loop ((code code))
      (if (null? code)
        (joy-ok #t)
        (let ((token (car code)))
          (cond
            ((eq? token 'define)
              (joy-bind (rest (joy-define-all (cdr code)))
                (loop rest)))
            (else
              (let ((res (joy-exec-one token)))
                (if (joy-ok? res) (loop (cdr code)) res)))))))))

;; Executes a single instruction: evaluates defined words, invokes primitives,
;; expands quotations, or pushes literal data onto the stack.
(define (joy-exec-one token)
  (cond
    ((symbol? token)
      (let ((val (joy-get token)))
        (cond
          ((eq? val joy-undefined)
            (joy-err
              (string-append "Undefined symbol: " (symbol->string token))))
          ((procedure? val) (val))
          ((list? val) (joy-exec val))
          (else (joy-push! val) (joy-ok #t)))))
    (else (joy-push! token) (joy-ok #t))))

;; Runs QUOT with ARGS pushed on top of the current stack (the last one
;; topmost), takes the top item QUOT leaves, and puts the stack back as it
;; was before ARGS went on. Returns that item in a result.
(define (joy-apply-isolated quot args)
  (let ((snapshot (joy-snapshot)))
    (for-each joy-push! args)
    (let ((res (joy-seq (joy-exec quot) (joy-pop!))))
      (joy-restore! snapshot)
      res)))

;; Joy's false is #f or a numeric zero; everything else is true.
(define (joy-true? x)
  (not (or (eq? x #f) (and (number? x) (zero? x)))))

;; Runs the condition QUOT against the stack without consuming anything.
;; Returns #t or #f in a result.
(define (joy-test quot)
  (joy-bind (verdict (joy-apply-isolated quot '()))
    (joy-ok (joy-true? verdict))))

(define (joy-print-stack)
  (let ((items (joy-stack->list)))
    (unless (null? items)
      (display "Stack: ")
      (for-each (lambda (x) (joy-write x) (display " ")) (reverse items))
      (newline))))

(define (joy-write x)
  (cond ((eq? x #t) (display "true"))
    ((eq? x #f) (display "false"))
    ((char? x) (display "'") (display x))
    ((joy-var? x) (if (joy-free? x) (display "_") (joy-write (joy-walk x))))
    ((list? x)
      (display "[")
      (let loop ((l x))
        (unless (null? l)
          (joy-write (car l))
          (when (pair? (cdr l)) (display " "))
          (loop (cdr l))))
      (display "]"))
    (else (write x))))

;;; ---- Equality and order ----

;; Structural and numeric equality between two Joy values.
;; Normalizes booleans, numbers, and characters where appropriate.
(define (joy-equal? x y)
  (let ((x (joy-walk x)) (y (joy-walk y)))
    (cond
      ((and (boolean? x) (number? y)) (= (if x 1 0) y))
      ((and (number? x) (boolean? y)) (= x (if y 1 0)))
      ((and (number? x) (char? y)) (= x (char->integer y)))
      ((and (char? x) (number? y)) (= (char->integer x) y))
      ((and (number? x) (number? y)) (= x y))
      ((and (list? x) (list? y))
        (and (= (length x) (length y))
          (let loop ((l1 x) (l2 y))
            (or (null? l1)
              (and (joy-equal? (car l1) (car l2)) (loop (cdr l1) (cdr l2)))))))
      (else (equal? x y)))))

;; Tests if X is an element of list LST under joy-equal?.
(define (joy-member? x lst)
  (exists (lambda (m) (joy-equal? m x)) lst))

;; Maps booleans and characters to integers for uniform ordinal comparison.
(define (joy-ordinal x)
  (cond
    ((boolean? x) (if x 1 0))
    ((char? x) (char->integer x))
    (else x)))

;; Compares two values: returns -1, 0, or 1; #f if unorderable.
(define (joy-compare x y)
  (let ((a (joy-ordinal x)) (b (joy-ordinal y)))
    (cond
      ((and (real? a) (real? b))
        (cond ((< a b) -1) ((> a b) 1) (else 0)))
      ((and (string? a) (string? b))
        (cond ((string<? a b) -1) ((string>? a b) 1) (else 0)))
      ((and (symbol? a) (symbol? b))
        (joy-compare (symbol->string a) (symbol->string b)))
      (else #f))))

;; Applies comparison predicate TEST (<, >, <=, >=) to X and Y.
(define (joy-relation test x y)
  (let ((order (joy-compare x y)))
    (if order
      (test order 0)
      (joy-err "Values of these types cannot be ordered"))))

;; Returns members of a list or string as a list; #f for other types.
(define (joy-members x)
  (cond
    ((list? x) x)
    ((string? x) (string->list x))
    (else #f)))

;; Takes the first N elements of LST.
(define (joy-list-take n lst)
  (if (or (<= n 0) (null? lst))
    '()
    (cons (car lst) (joy-list-take (- n 1) (cdr lst)))))

;; Drops the first N elements of LST.
(define (joy-list-drop n lst)
  (if (or (<= n 0) (null? lst))
    lst
    (joy-list-drop (- n 1) (cdr lst))))

;; Pushes each member in turn and executes quotation QUOT after it.
(define (joy-step-members quot members)
  (let loop ((in members))
    (if (null? in)
      (joy-ok #t)
      (begin
        (joy-push! (car in))
        (joy-seq (joy-exec quot) (loop (cdr in)))))))

;;; ---- Primitives: stack ----

(joy-prim-void (dot x) (joy-write x) (newline))
(joy-set! tok-end (joy-get 'dot))

(joy-prim-void (stack) (joy-push! (joy-stack->list)))
(joy-prim-void (env) (joy-push! joy-env))
(joy-prim-void (peek)
  (joy-bind (top (joy-peek))
    (joy-push! top)))
(joy-prim-void (size) (joy-push! (joy-stack-size)))
(joy-prim-void (clear) (joy-clear-stack!))

(joy-prim-list (dup x) (list x x))
(joy-prim-void (pop x) (joy-ok #t))
(joy-prim-void (id) (joy-ok #t))
(joy-prim-list (swap top next) (list next top))
(joy-prim-list (rotate top next third) (list third next top))

(joy-prim-list (dup2 top next) (list top next top next))
(joy-prim-void (pop2 top next) (joy-ok #t))
(joy-prim-list (swap2 t n3 n2 n1) (list n2 n1 t n3))

(joy-prim-void (rollup top next third)
  (joy-push! top)
  (joy-push! third)
  (joy-push! next))
(joy-prim-void (rolldown top next third)
  (joy-push! next)
  (joy-push! top)
  (joy-push! third))

(joy-prim-list (over top next) (list next top next))

;; N counts down from the top after N is popped; 0 pick is equivalent to dup.
(joy-prim-list (pick n)
  (let ((items (joy-stack->list)))
    (if (and (integer? n) (exact? n) (< -1 n (length items)))
      (list (list-ref items n))
      (joy-err "pick index out of bounds"))))
(joy-prim-list (tuck top next) (list top next top))
(joy-prim (nip top next) top)

;;; ---- Primitives: arithmetic ----

(joy-prim (+ top next)
  (let ((x next) (y top))
    (cond
      ((and (number? x) (number? y)) (+ x y))
      ((and (char? x) (integer? y))
        (integer->char (+ (char->integer x) y)))
      (else
        (joy-err
          "type error: + expects two numbers or a char and an integer")))))

(joy-prim (- top next)
  (let ((x next) (y top))
    (cond
      ((and (number? x) (number? y)) (- x y))
      ((and (char? x) (integer? y))
        (integer->char (- (char->integer x) y)))
      ((and (char? x) (char? y))
        (- (char->integer x) (char->integer y)))
      (else
        (joy-err
          "type error: - expects numbers, char+int, or two chars")))))

(joy-prim (* top next)
  (if (and (number? top) (number? next))
    (* next top)
    (joy-err "type error: * expects numbers")))

(joy-prim (/ top next)
  (let ((x next) (y top))
    (cond
      ((not (and (number? x) (number? y)))
        (joy-err "type error: / expects numbers"))
      ((zero? y) (joy-err "Division by zero"))
      ((and (exact? x) (integer? x) (exact? y) (integer? y))
        (quotient x y))
      (else (/ (inexact x) (inexact y))))))

;; Computes quotient and remainder: leaves quotient below, remainder on top.
(joy-prim-list (div top next)
  (cond
    ((not (and (integer? top) (integer? next)))
      (joy-err "type error: div expects integers"))
    ((zero? top) (joy-err "Division by zero"))
    (else (list (remainder next top) (quotient next top)))))

(joy-prim (rem top next)
  (cond
    ((not (and (integer? top) (integer? next)))
      (joy-err "type error: rem expects integers"))
    ((zero? top) (joy-err "Division by zero"))
    (else (remainder next top))))

(joy-prim (abs x)
  (if (number? x) (abs x) (joy-err "abs expects number")))
(joy-prim (neg x)
  (if (number? x) (- x) (joy-err "neg expects number")))
(joy-prim (sign x)
  (if (number? x)
    (cond ((> x 0) 1) ((< x 0) -1) (else 0))
    (joy-err "sign expects number")))
(joy-prim (max top next)
  (if (and (number? top) (number? next))
    (if (>= next top) next top)
    (joy-err "max expects numbers")))
(joy-prim (min top next)
  (if (and (number? top) (number? next))
    (if (<= next top) next top)
    (joy-err "min expects numbers")))
(joy-prim (sqrt x)
  (if (number? x) (sqrt x) (joy-err "sqrt expects number")))
(joy-prim (pow top next)
  (if (and (number? top) (number? next))
    (expt next top)
    (joy-err "pow expects numbers")))

(joy-prim (succ x)
  (cond
    ((char? x) (integer->char (+ (char->integer x) 1)))
    ((number? x) (+ x 1))
    (else (joy-err "succ expects char or number"))))
(joy-prim (pred x)
  (cond
    ((char? x) (integer->char (- (char->integer x) 1)))
    ((number? x) (- x 1))
    (else (joy-err "pred expects char or number"))))

;;; ---- Primitives: comparison and logic ----

(joy-prim (= top next) (joy-equal? next top))
(joy-prim (equal top next) (joy-equal? next top))
(joy-prim (eql top next) (joy-equal? next top))
(joy-prim (< top next) (joy-relation < next top))
(joy-prim (> top next) (joy-relation > next top))
(joy-prim (<= top next) (joy-relation <= next top))
(joy-prim (>= top next) (joy-relation >= next top))
(joy-prim (not= top next) (not (joy-equal? next top)))
(joy-prim (neql top next) (not (joy-equal? next top)))
(joy-prim (compare top next)
  (or (joy-compare next top)
    (joy-err "Values of these types cannot be ordered")))

(joy-prim (zero? x)
  (if (number? x) (= x 0) #f))
(joy-prim (pos? x)
  (if (number? x) (> x 0) #f))
(joy-prim (neg? x)
  (if (number? x) (< x 0) #f))
(joy-prim (odd? x)
  (if (integer? x) (odd? x) #f))
(joy-prim (even? x)
  (if (integer? x) (even? x) #f))
(joy-prim (small x)
  (cond
    ((list? x) (< (length x) 2))
    ((string? x) (< (string-length x) 2))
    ((integer? x) (or (= x 0) (= x 1)))
    (else #f)))

(joy-prim (and top next)
  (let ((x next) (y top))
    (cond
      ((and (boolean? x) (boolean? y)) (and x y))
      ((and (integer? x) (integer? y)) (bitwise-and x y))
      (else (joy-err "and expects booleans or integers")))))
(joy-prim (or top next)
  (let ((x next) (y top))
    (cond
      ((and (boolean? x) (boolean? y)) (or x y))
      ((and (integer? x) (integer? y)) (bitwise-ior x y))
      (else (joy-err "or expects booleans or integers")))))
(joy-prim (xor top next)
  (let ((x next) (y top))
    (cond
      ((and (boolean? x) (boolean? y)) (not (eq? x y)))
      ((and (integer? x) (integer? y)) (bitwise-xor x y))
      (else (joy-err "xor expects booleans or integers")))))
(joy-prim (not x)
  (cond
    ((boolean? x) (not x))
    ((integer? x) (bitwise-not x))
    (else (joy-err "not expects boolean or integer"))))

(joy-prim (integer x) (integer? x))
(joy-prim (boolean x) (boolean? x))
(joy-prim (list x) (list? x))
(joy-prim (symbol x) (symbol? x))
(joy-prim (char x) (char? x))
(joy-prim (string x) (string? x))
(joy-prim (null x)
  (cond
    ((null? x) #t)
    ((and (string? x) (zero? (string-length x))) #t)
    ((and (number? x) (zero? x)) #t)
    ((and (boolean? x) (not x)) #t)
    (else #f)))

(joy-prim (ord x)
  (if (char? x) (char->integer x) (joy-err "ord expects char")))
(joy-prim (chr x)
  (if (integer? x) (integer->char x) (joy-err "chr expects int")))

;;; ---- Primitives: aggregates ----

(joy-prim (cons top next)
  (if (list? top)
    (cons next top)
    (joy-err "cons expects a list")))

(joy-prim (swons top next)
  (if (list? next)
    (cons top next)
    (joy-err "swons expects a list")))

(joy-prim (snoc top next)
  (cond
    ((list? next) (append next (list top)))
    ((and (string? next) (char? top))
      (string-append next (string top)))
    (else (joy-err "snoc expects list or string+char"))))

;; Deconstructs a sequence from the front:
;; leaves the rest on top, first item below.
(joy-prim-list (uncons x)
  (cond
    ((pair? x)
      (list (cdr x) (car x)))
    ((and (string? x) (> (string-length x) 0))
      (list (substring x 1 (string-length x)) (string-ref x 0)))
    (else (joy-err "uncons expects a non-empty list or string"))))

;; Deconstructs a sequence from the front:
;; leaves the first item on top, rest below.
(joy-prim-list (unswons x)
  (cond
    ((pair? x)
      (list (car x) (cdr x)))
    ((and (string? x) (> (string-length x) 0))
      (list (string-ref x 0) (substring x 1 (string-length x))))
    (else (joy-err "unswons expects a non-empty list or string"))))

;; Deconstructs from the back: leaves initial items on top, last item below.
(joy-prim-list (uncons-bot x)
  (cond
    ((and (list? x) (not (null? x)))
      (let ((r (reverse x)))
        (list (reverse (cdr r)) (car r))))
    ((and (string? x) (> (string-length x) 0))
      (let ((n (string-length x)))
        (list (substring x 0 (- n 1)) (string-ref x (- n 1)))))
    (else (joy-err "uncons-bot expects a non-empty list or string"))))

(joy-prim (first x)
  (cond
    ((pair? x) (car x))
    ((and (string? x) (> (string-length x) 0)) (string-ref x 0))
    (else (joy-err "first expects a non-empty list or string"))))

(joy-prim (rest x)
  (cond
    ((pair? x) (cdr x))
    ((and (string? x) (> (string-length x) 0))
      (substring x 1 (string-length x)))
    (else (joy-err "rest expects a non-empty list or string"))))

(joy-prim (length x)
  (cond
    ((list? x) (length x))
    ((string? x) (string-length x))
    (else 0)))

(joy-prim (append top next)
  (let ((x next) (y top))
    (cond
      ((and (list? x) (list? y)) (append x y))
      ((and (string? x) (string? y)) (string-append x y))
      (else (joy-err "append expects two lists or two strings")))))

(joy-prim (reverse x)
  (cond
    ((list? x) (reverse x))
    ((string? x) (list->string (reverse (string->list x))))
    (else x)))

(joy-prim (take top next)
  (if (and (integer? top) (list? next))
    (joy-list-take top next)
    (joy-err "take expects int and list")))

(joy-prim (drop top next)
  (if (and (integer? top) (list? next))
    (joy-list-drop top next)
    (joy-err "drop expects int and list")))

(joy-prim (at top next)
  (if (and (integer? top) (exact? top) (list? next))
    (if (< -1 top (length next))
      (list-ref next top)
      (joy-err "at index out of bounds"))
    (joy-err "at expects int and list")))

;; Sets are represented algebraically as integer bitmasks.
(joy-prim (union top next)
  (if (and (integer? top) (integer? next))
    (bitwise-ior next top)
    (joy-err "union expects set integers")))

(joy-prim (intersection top next)
  (if (and (integer? top) (integer? next))
    (bitwise-and next top)
    (joy-err "intersection expects set integers")))

(joy-prim (difference top next)
  (if (and (integer? top) (integer? next))
    (bitwise-and next (bitwise-not top))
    (joy-err "difference expects set integers")))

;; X A in: returns #t if element X is in set or aggregate A.
(joy-prim (in top next)
  (cond
    ((and (integer? next) (integer? top))
      (bit-set? top next))
    ((list? top) (joy-member? next top))
    (else #f)))

;; A X has: returns #t if set or aggregate A contains element X.
(joy-prim (has top next)
  (cond
    ((and (integer? next) (integer? top))
      (bit-set? next top))
    ((list? next) (joy-member? top next))
    (else #f)))

;; Tests whether bit index N is set in bitmask SET.
(define (bit-set? set n)
  (and (>= n 0)
    (not (zero? (bitwise-and set (bitwise-arithmetic-shift-left 1 n))))))

;; Prints all defined symbols in alphabetical order.
(joy-prim-void (words)
  (for-each (lambda (name) (display name) (display " "))
    (list-sort
      (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
      (vector->list (hashtable-keys joy-env))))
  (newline))

(joy-prim-void (print x) (joy-write x) (newline))
(joy-prim-void (put x) (joy-write x) (display " "))

;;; ---- Combinators ----

(joy-prim-void (i quot)
  (if (list? quot)
    (joy-exec quot)
    (joy-err "i expects a quotation")))

;; X [P] dip: executes P while keeping X temporarily removed from the stack.
(joy-prim-void (dip quot)
  (if (list? quot)
    (joy-bind (x (joy-pop!))
      (let ((res (joy-exec quot)))
        (joy-push! x)
        res))
    (joy-err "dip expects a quotation")))

;; Y X [P] dip2: executes P while keeping top two elements set aside.
(joy-prim-void (dip2 quot top next)
  (if (list? quot)
    (let ((res (joy-exec quot)))
      (joy-push! next)
      (joy-push! top)
      res)
    (joy-err "dip2 expects a quotation")))

;; [P] [T] [E] ifte: evaluates condition P in an isolated sandbox.
;; If P produces true (or non-zero), branch T runs; otherwise branch E runs.
(joy-prim-void (ifte e t p)
  (joy-bind (go? (joy-test p))
    (joy-exec (if go? t e))))

;; B [T] [E] branch: tests already-computed boolean B, choosing T or E.
(joy-prim-void (branch e t b)
  (joy-exec (if (joy-true? b) t e)))

;; Multi-branch condition combinator: [ [P1 T1] [P2 T2] ... [Default] ] cond
(joy-prim-void (cond clauses)
  (let loop ((c clauses))
    (cond
      ((null? c) (joy-ok #t))
      ((null? (cdr c)) (joy-exec (car c)))
      (else
        (joy-bind (go? (joy-test (caar c)))
          (if go?
            (joy-exec (cdar c))
            (loop (cdr c))))))))

;; Applies quotation QUOT to each element of LST in isolation,
;; collecting the results into a new list.
(joy-prim-void (map quot lst)
  (if (list? lst)
    (let loop ((in lst) (out '()))
      (if (null? in)
        (begin
          (joy-push! (reverse out))
          (joy-ok #t))
        (joy-bind (item (joy-apply-isolated quot (list (car in))))
          (loop (cdr in) (cons item out)))))
    (joy-err "map expects a list")))

;; Filters list LST with predicate quotation QUOT.
(joy-prim-void (filter quot lst)
  (if (list? lst)
    (let loop ((in lst) (out '()))
      (if (null? in)
        (begin
          (joy-push! (reverse out))
          (joy-ok #t))
        (joy-bind (item (joy-apply-isolated quot (list (car in))))
          (loop (cdr in)
            (if (joy-true? item)
              (cons (car in) out)
              out)))))
    (joy-err "filter expects a list")))

;; Iterates over elements of aggregate OBJ executing QUOT for each.
(joy-prim-void (step quot obj)
  (let ((members (joy-members obj)))
    (if members
      (joy-step-members quot members)
      (joy-err "step expects a list or string"))))

;; Left-folds aggregate OBJ starting from INIT with binary quotation QUOT.
(joy-prim-void (fold quot init obj)
  (let ((members (joy-members obj)))
    (if members
      (begin
        (joy-push! init)
        (joy-step-members quot members))
      (joy-err "fold expects a list or string"))))

;; Repeats execution of QUOT N times.
(joy-prim-void (times quot n)
  (if (integer? n)
    (let loop ((count n))
      (if (<= count 0)
        (joy-ok #t)
        (joy-seq (joy-exec quot) (loop (- count 1)))))
    (joy-err "times expects an integer count")))

;; Repeatedly executes BODY while condition TEST yields true.
(joy-prim-void (while body test)
  (let loop ()
    (joy-bind (go? (joy-test test))
      (if go?
        (joy-seq (joy-exec body) (loop))
        (joy-ok #t)))))

;; Executes QUOT without input arguments and pushes its single output value.
(joy-prim-void (nullary quot)
  (joy-bind (r (joy-apply-isolated quot '()))
    (joy-push! r)))

;; Executes QUOT on argument X and pushes its single output value.
(joy-prim-void (unary quot x)
  (joy-bind (r (joy-apply-isolated quot (list x)))
    (joy-push! r)))

;; Executes QUOT on arguments X and Y and pushes its single output value.
(joy-prim-void (binary quot x y)
  (joy-bind (r (joy-apply-isolated quot (list y x)))
    (joy-push! r)))

;; X [P1] [P2] cleave: applies P1 and P2 to X independently.
(joy-prim-void (cleave q2 q1 x)
  (joy-bind (r1 (joy-apply-isolated q1 (list x)))
    (joy-bind (r2 (joy-apply-isolated q2 (list x)))
      (joy-push! r1)
      (joy-push! r2))))

;; X Y [P1] [P2] spread: applies P1 to X and P2 to Y independently.
(joy-prim-void (spread q2 q1 y x)
  (joy-bind (r1 (joy-apply-isolated q1 (list x)))
    (joy-bind (r2 (joy-apply-isolated q2 (list y)))
      (joy-push! r1)
      (joy-push! r2))))

;; Executes QUOT within a temporary stack initialized from list LST.
(joy-prim-void (infra quot lst)
  (if (list? lst)
    (let ((snapshot (joy-snapshot)))
      (joy-clear-stack!)
      (joy-push-list! lst)
      (let* ((res (joy-exec quot))
             (inner (joy-stack->list)))
        (joy-restore! snapshot)
        (if (joy-ok? res)
          (joy-push! inner)
          res)))
    (joy-err "infra expects a list")))

;;; ---- Recursion combinators ----

;; Linear recursion combinator: [P] [T] [R1] [R2] linrec
(joy-prim-void (linrec r2 r1 t p)
  (let rec ()
    (joy-bind (go? (joy-test p))
      (if go?
        (joy-exec t)
        (joy-seq (joy-exec r1) (rec) (joy-exec r2))))))

;; Binary recursion combinator: [P] [T] [R1] [R2] binrec
(joy-prim-void (binrec r2 r1 t p)
  (let rec ()
    (joy-bind (go? (joy-test p))
      (if go?
        (joy-exec t)
        (joy-seq
          (joy-exec r1)
          (rec)
          (joy-exec '(swap))
          (rec)
          (joy-exec r2))))))

;; Primitive recursion on non-negative integers or sequences.
(joy-prim-void (primrec step init x)
  (cond
    ((integer? x)
      (let loop ((n x))
        (if (<= n 0)
          (joy-exec init)
          (begin
            (joy-push! n)
            (joy-seq (loop (- n 1)) (joy-exec step))))))
    ((joy-members x)
      => (lambda (members)
           (let loop ((l members))
             (if (null? l)
               (joy-exec init)
               (begin
                 (joy-push! (car l))
                 (joy-seq (loop (cdr l)) (joy-exec step)))))))
    (else (joy-err "primrec expects integer, list or string"))))

;; General recursion combinator:
;; pushes recursion quotation for manual re-entry.
(joy-prim-void (genrec r2 r1 t p)
  (joy-bind (go? (joy-test p))
    (if go?
      (joy-exec t)
      (joy-seq
        (joy-exec r1)
        (begin
          (joy-push! (list p t r1 r2 'genrec))
          (joy-ok #t))
        (joy-exec r2)))))

;; Tree recursion combinator: traverses nested lists recursively.
(joy-prim-void (treerec comb leaf)
  (let rec ()
    (joy-bind (item (joy-pop!))
      (if (list? item)
        (let loop ((in item) (out '()))
          (if (null? in)
            (begin
              (joy-push! (reverse out))
              (joy-exec comb))
            (begin
              (joy-push! (car in))
              (joy-seq (rec)
                (joy-bind (res (joy-pop!))
                  (loop (cdr in) (cons res out)))))))
        (begin
          (joy-push! item)
          (joy-exec leaf))))))

;;; ---- Logic primitives ----

(joy-prim-void (hole)
  (joy-push! (make-joy-var joy-unbound)))

;; Pops two items, unifies them, and pushes boolean status (#t or #f).
(joy-prim-void (unify)
  (joy-bind (u (joy-pop!))
    (joy-bind (v (joy-pop!))
      (joy-push! (joy-try-unify u v)))))

;; Unifies the top item with the item at the bottom of the deque.
(joy-prim-void (unify-ends)
  (joy-bind (u (joy-pop!))
    (joy-bind (v (joy-shift!))
      (joy-push! (joy-try-unify u v)))))

;; Logical ifte: rolls back bindings made during TEST if condition fails.
(joy-prim-void (lifte else-branch then-branch test-quot)
  (let ((mark joy-trail))
    (joy-bind (go? (joy-test test-quot))
      (if go?
        (joy-exec then-branch)
        (begin
          (joy-rollback! mark)
          (joy-exec else-branch))))))

;;; ---- Deque primitives ----

;; Shifts the bottom item from the deque and pushes it to the top.
(joy-prim-void (pull)
  (joy-bind (bottom (joy-shift!))
    (joy-push! bottom)))

(joy-prim-void (put-bot x)
  (joy-unshift! x))

(joy-prim-void (mode)
  (joy-push! joy-mode))

(joy-prim-void (set-mode m)
  (set! joy-mode m))

;;; ---- Files ----

;; Searches for library file in search paths ("", "lib/", "../PY_JOY/").
(define (joy-find-library name)
  (let loop ((dirs '("" "lib/" "../PY_JOY/")))
    (cond
      ((null? dirs) #f)
      ((file-exists? (string-append (car dirs) name))
        (string-append (car dirs) name))
      (else (loop (cdr dirs))))))

;; Evaluates an external Joy source file.
(joy-prim-void (include filename)
  (cond
    ((not (string? filename))
      (joy-err "include expects a string filename"))
    ((joy-find-library filename)
      => (lambda (path)
           (joy-eval-text (call-with-input-file path get-string-all))))
    (else
      (joy-err (string-append "include file not found: " filename)))))

;;; ---- Aliases and library words ----

(joy-alias! 'concat 'append)
(joy-alias! 'small? 'small)
(joy-alias! 'mod 'rem)

(joy-define dupd [dup] dip)
(joy-define swapd [swap] dip)
(joy-define rolldownd [rolldown] dip)
(joy-define rollupd [rollup] dip)
(joy-define popd [pop] dip)

;; Tagged result sum types: [ok X] and [err E].
;; Monadic combinators map-ok and bind-ok operate in pure point-free style.
(joy-define ok [] cons [ok] swap concat)
(joy-define err [] cons [err] swap concat)
(joy-define is-ok? first [ok] first =)
(joy-define is-err? first [err] first =)
(joy-define unwrap rest first)
(joy-define map-ok [is-ok?] [unwrap swap i ok] [popd] ifte)
(joy-define bind-ok [is-ok?] [unwrap swap i] [popd] ifte)

;;; ---- Running programs ----

(define (joy-describe-exception ex)
  (call-with-string-output-port
    (lambda (port)
      (if (condition? ex)
        (display-condition ex port)
        (write ex port)))))

;; Traps host Scheme exceptions and maps them to Joy error results.
(define (joy-guarded-exec code)
  (guard (ex (else (joy-err (joy-describe-exception ex))))
    (joy-exec code)))

;; Evaluates an entire program text; incomplete forms are reported as errors.
(define (joy-eval-text text)
  (let ((code (joy-tokenize text)))
    (if (joy-incomplete? code)
      (joy-err (string-append "Unexpected end of input: " (joy-res-val code)))
      (joy-guarded-exec code))))

(define (joy-report-error msg)
  (display "ERROR: ")
  (display msg)
  (newline))

;; Interactive Read-Eval-Print Loop with multi-line buffering.
(define (joy-repl)
  (let loop ((pending ""))
    (display (if (string=? pending "") "> " ">> "))
    (flush-output-port (current-output-port))
    (let ((line (get-line (current-input-port))))
      (if (eof-object? line)
        (display "\nGoodbye.\n")
        (let* ((text (string-append pending line "\n"))
               (code (joy-tokenize text)))
          (if (joy-incomplete? code)
            (loop text)
            (let ((res (joy-guarded-exec code)))
              (if (joy-err? res)
                (joy-report-error (joy-res-val res))
                (joy-print-stack))
              (loop ""))))))))

;; Executes a file or standard input.
(define (joy-run-file filename)
  (cond
    ((and (not (string=? filename "-")) (not (file-exists? filename)))
      (joy-report-error (string-append "file not found: " filename))
      (exit 1))
    (else
      (let ((res (joy-eval-text
                   (if (string=? filename "-")
                     (get-string-all (current-input-port))
                     (call-with-input-file filename get-string-all)))))
        (cond
          ((joy-err? res)
            (joy-report-error (joy-res-val res))
            (exit 1))
          (else (joy-print-stack)))))))

;;; ---- Entry point ----

(let ((args (command-line-arguments)))
  (if (null? args)
    (begin
      (display "SC_Joy. Type 'words' to list commands.\n")
      (joy-repl))
    (joy-run-file (car args))))

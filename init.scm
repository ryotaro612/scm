

;;; 大域環境のシンボルの管理
(define *global-env* '())

;;; 大域環境のマクロの管理
(define *macro-symbols* '())

;;; エラー時の大域脱出のための関数
(define init "abort")

(define (init-global-env)
  (list
   (cons 'number?        number?)
   (cons '+              +)
   (cons '-              -)
   (cons '*              *)
   (cons '/              /)
   (cons '=              =)
   (cons '<              <)
   (cons '<=             <=)
   (cons '>              >)
   (cons '>=             >=)   
   (cons 'null?          null?)
   (cons 'pair?          pair?)
   (cons 'list?          list?)
   (cons 'symbol?        symbol?)
   (cons 'car            car)
   (cons 'cdr            cdr)
   (cons 'cons           cons)
   (cons 'list           list)
   (cons 'length         length)
   (cons 'memq           memq)
   (cons 'last           last)
   (cons 'append         append)
   (cons 'set-car!       set-car!)
   (cons 'set-cdr!       set-cdr!)
   (cons 'boolean?       boolean?)
   (cons 'not            not)
   (cons 'string?        string?)
   (cons 'string-append  string-append)
   (cons 'symbol->string symbol->string)
   (cons 'string->symbol string->symbol)
   (cons 'string-number  string->number)
   (cons 'number->string number->string)
   (cons 'procedure?     procedure?)
   (cons 'eq?            eq?)
   (cons 'neq?           neq?)
   (cons 'equal?         equal?)
   (cons 'load           my-load) ; load 
   (cons 'call/cc        call/cc)
   ))
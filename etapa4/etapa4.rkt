#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))
(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))
;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).
(define (func cuv1 cuv2 L)
  (if (or (null? cuv1) (null? cuv2))
      (cons L (cons cuv1 (cons cuv2 '())))
      (if (eq? (car cuv1) (car cuv2))
          (func (cdr cuv1) (cdr cuv2) (append L (list (car cuv1))))
           (cons L (cons cuv1 (cons cuv2 '()))))))

(define (longest-common-prefix w1 w2)
  (func w1 w2 '()))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (prfx cuv1 cuv2 L)
  (if (or (null? cuv1) (null? cuv2))
      (reverse L)
      (if (eq? (car cuv1) (car cuv2))
          (prfx (cdr cuv1) (cdr cuv2) (cons (car cuv1) L))
          (reverse L))))

(define (longest-common-prefix1 prefix words)
    (if (stream-empty? words)
        prefix
        (longest-common-prefix1 (prfx prefix (collection-first words) '()) (collection-rest words))))

(define (longest-common-prefix-of-collection words)
   (longest-common-prefix1 (collection-first words) (collection-rest words)))

;j

(define (sufix word prefix)
  (drop word (length prefix)))

(define (f tree pattern L)
  (if (null? tree)
      '(#f ())
      (if (equal? pattern (prfx (collection-first (collection-first tree)) pattern L))
          #t
          (if (equal? (collection-first (collection-first tree)) (prfx (collection-first (collection-first tree)) pattern L))
              (collection-cons (collection-first (collection-first tree)) (collection-cons (sufix pattern (prfx (collection-first (collection-first tree)) pattern L)) (list(collection-rest (collection-first tree)))))
              (if (not (equal? '() (prfx (collection-first (collection-first tree)) pattern L)))
                  (collection-cons '#f (list (append (prfx (collection-first (collection-first tree)) pattern L) L)))
                  (f (collection-rest tree) pattern L))))))
   
(define (match-pattern-with-label st pattern)
  (f st pattern '()))
;
(define (st-has-pattern? st pattern)
  (if (null? st)
      #f
      (if (or(equal? pattern (prfx pattern (car (car st)) '())) (equal? pattern (car (car st))))
          #t
          (if (not (null? (prfx pattern (car (car st)) '())))
              (st-has-pattern? (cdar st) (sufix pattern (prfx pattern (car (car st)) '())))
              (st-has-pattern? (cdr st) pattern)))))


(define (get-suffixes text)
 (if (null? text)
     '()
     (collection-cons text (get-suffixes (cdr text)))))


(define (get-ch-words words ch)
  (collection-filter (lambda (words) (and (not(null? words)) (equal? ch (collection-first words)))) words))



(define (ast-func suffixes)
  (if (stream-empty? suffixes) empty-stream
      (cons (list(car (collection-first suffixes))) (collection-map (lambda (suffixes) (collection-rest suffixes)) suffixes))))


(define (cst-func suffixes)
  (if (stream-empty? suffixes) empty-stream
  (cons (longest-common-prefix-of-collection suffixes) (collection-map (lambda (word) (drop word (length (longest-common-prefix-of-collection suffixes)))) suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (collection-filter (lambda (list) (not (null? list)))
          (collection-map (lambda (c)
                 (if (not (null? (get-ch-words suffixes c)))
                     (append (list (car (labeling-func (get-ch-words suffixes c)))) (suffixes->st labeling-func (cdr (labeling-func (get-ch-words suffixes c))) alphabet))
                     '()))
               alphabet)))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (suffixes->st labeling-func (get-suffixes (append text '(#\$))) (sort(remove-duplicates (append text '(#\$))) char<?)))))



(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))


(define text->cst
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)
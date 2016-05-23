;;; helm-complete-tag.el --- complete tag mehod with helm interface

;; Copyright © 2015  Maxim Musolov <mmusolov@gmail.com>

;; Author: Maxim Musolov <mmusolov@gmail.com>
;; URL: https://github.com/rhexo/helm-complete-tag
;; Keywords: tags, completion, assistant
;; Version: 0.0.0
;; Package-Requires: 

;; * helm

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;;  Restrictions: only for c/C++ context
;;
;;  1. using etags file to create a list of candidates
;;  2. using helm interface to display/paste candidates that returns at 1st step
;;
;;; Code:

(require 'helm)
(require 'projectile)


;; Реализуемые механизмы
;; 1. поиск кандидатов
;;   * для завершения ввода переменной V, а так же типа данных T (слева от курсора выражение не разделенное пробелами)
;;   * для элемента типа T, связанного с переменной V (курсор следует за символами {'::';'.';'->'})
;;   * для объявления типа (в текущем положении курсора, если слева и справа от его позиция имеет значение пусто)
;; 2. выполнение переходов
;;   * h/cpp по имени тэга для типа данных (определение <-> реализация) 
;;   * [C-c p s a] для переменной ( В идеале, выводить индекс использования переменной внутри тела функции, для глобальных переменных, фильтр по типу и вывод аналогичный [C-c p s a])


;; 23/05/2016
;; 1. Используем TAGS файл как индекс для множества понятий используемых в проекте.
;; 2. Накладываем фильтр на это множество понятий исходя из текущего положения курсора. Обобщенно, возможные ситуации были описаны в п1. ранее.
;; 3. Для оставшегося подмножества выполняем синтаксический анализ (если он ранее не был выполнен для tag-а) используя исходные файлы и номера строк из индекса TAGS
;;    При обновлении индекса TAGS, полностью обновляем буффер "разобранных" понятий


(defvar helm-complete-tag-table
  "parsed TAGS file entries")

(defvar helm-complete-tag-index
  "parsed TAGS file entries")


;;--------------------------------------------------------------------------------------

(defun helm-complete-tag-read-file (path)
  "read TAGS file as list of data"
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun helm-complete-tag-parse-index-data (file-data) 
    "Read TAGS file. Fill list from entries."
  (let ((file-str nil)
        (index-data nil)
        (tag-text nil)
        (tag-key nil)
        (tag-index)
        (tag-shift)
        (tag-str)
        (tag-file))

    (while file-data 

      (setq file-str (car file-data))
      ;; break condition
      (if (eq (string-to-char file-str) ?\^L)

          (progn
          ;; scrolls TAGS heads info
            (setq file-data (cdr file-data))
            (setq tag-file (replace-regexp-in-string "," "" (when (string-match "\\(.*\\)," (car file-data))
                                                                (match-string 0 (car file-data)))))
          (setq file-data (cdr (cdr file-data))))

        (progn                      
          (setq tag-str (split-string file-str ""))
          (setq tag-text (car tag-str)) ;; tag-text
          (setq file-str (car (cdr tag-str)))
          (setq tag-str (split-string file-str ""))
          (setq tag-key (car tag-str)) ;; tag-key
          (setq file-str (car (cdr tag-str)))
          (setq tag-str (split-string file-str ","))
          (setq tag-index (car tag-str)) ;; tag-index 
          (setq tag-shift (car (cdr tag-str))) ;; tag-shift / position in file

          (add-to-list 'index-data (list tag-key tag-file tag-index tag-shift))
          (setq file-data (cdr file-data)))))

    ;; result
    index-data
    )
  )

      ;; const char features[] = {""features2,1
      ;; -> typeCat:data dataTag rowIndex dataData
      
      



    ;; (if (eq (string-to-char file-str) ?\^L)
    ;;       ;; new file - new structure
    ;;       (progn
    ;;         ;; get full path
    ;;         (setq file-data (cdr file-data))            
    ;;         (setq file-path (replace-regexp-in-string "," "" (when (string-match "\\(.*\\)," (car file-data))
    ;;                                                            (match-string 0 (car file-data)))))
    ;;         ;; get file name
    ;;         (setq file-data (cdr file-data))
    ;;         (setq file-name (replace-regexp-in-string "[]" "" (when (string-match "\\(.*\\)" (car file-data))
    ;;                                                                 (match-string 0 (car file-data)))))            
    ;;         ;; get file type
    ;;         (if (string-match "\\(\\.hpp$\\)\\|\\(\\.hh$\\)\\|\\(\\.hxx$\\)\\|\\(\\.h$\\)" file-name)
    ;;           (setq file-type "h")
    ;;           (setq file-type "cpp"))

    ;;         (setq file-data (cdr file-data))

    ;;         (let ((tag-group nil)
    ;;               (tag-text)
    ;;               (tag-key)
    ;;               (tag-index)
    ;;               (break-cond-p nil))

    ;;           (while (and (not break-cond-p) file-data )
    ;;             (setq file-line (car file-data))
    ;;             ;; break condition
    ;;             (if (eq (string-to-char file-line) ?\^L)
    ;;                 (setq break-cond-p t))

    ;;             ;; parse file structure
               
    ;;             (setq tag-text (car (split-string file-line ?\^?))) ;; tag-text
    ;;             (setq file-line (cdr (split-string file-line ?\^?)))
    ;;             (setq tag-key (car (split-string file-line ?\^A))) ;; tag-key
    ;;             (setq tag-index (cdr (split-string file-line ?\^A))) ;; tag-index (row,diff); diff from begining of file

    ;;             ;; const char features[] = {""features2,1
    ;;             ;; -> typeCat:data dataTag rowIndex dataData
                
                
    ;;             )))))


    ;; ))


;; helm-complete-tag-table structure
;;  (source:file path type={h,cpp}
;;      (namespace namespacetag-1 rowIndex
;;         (typeCat:type typeTag rowIndex typeData) 
;;         (typeCat:data dataTag rowIndex dataData) 
;;         (typeCat:struct/class strucTypeTag rowIndex
;;              (cat:def
;;                  (memCat:type strucMemberTag-1 rowIndex strucMemberData-1)
;;                  (memCat:data strucMemberTag-2 rowIndex strucMemberData-2)
;;                  (memCat:method strucMemberTag-3 rowIndex strucMemberData-3)
;;                  (memCat:friend strucMemberTag-4 rowIndex  strucMemberData-4))
;;              (cat:impl
;;                  (memcat:implData strucMemberTag-2 rowIndex strucMemberData-2)
;;                  (memcat:implMethod strucMembertag-3 rowIndex strucMemberData-3)))
;;          (typeCat:func funcTypeTag-1 rowIndex funcTypeData-1)
;;          (typecat:funcImpl funcTypeTag-1 rowIndex funcTypeData-1)
;;          (typeCat:macro macroTypeTag rowIndex macroTypeData)
;;          (namespace namespacetag-2 rowIndex
;;              (...)
;;          )
;;       )
;;   )


;; (defun helm-complete-tag-parse-get-typecat (str)
;;   "return str typecat of C/C++ context"
;;   (let ((result-p nil)) 
;;     ;; is type?
;;     (cond ((string-match "\\(^\s*typedef\s+\\)" str) (setq result-p 'type)) ;; str is type definition
;;           ((string-match "\\(^\s*struct\s+\\)\\|\\(^\s*class\s+\\)" str) (setq result-p 'struct)) ;; str is struct definition
;;           (t (setq result-p) 'data)))) ;; by default

;; (defun helm-complete-tag-parse-data (file-data)
;;   "parse list of data from TAGS file. Generate helm-complete-tag-table structure"
;;   (let ((file-line nil)
;;         (file-path nil)
;;         (file-name nil)
;;         (file-type nil))
;;     (while file-data      
;;       (setq file-line (car file-data))
;;       ;;(message "char %s" (string-to-char item))
;;       (if (eq (string-to-char file-line) ?\^L)
;;           ;; new file - new structure
;;           (progn
;;             ;; get full path
;;             (setq file-data (cdr file-data))            
;;             (setq file-path (replace-regexp-in-string "," "" (when (string-match "\\(.*\\)," (car file-data))
;;                                                                (match-string 0 (car file-data)))))
;;             ;; get file name
;;             (setq file-data (cdr file-data))
;;             (setq file-name (replace-regexp-in-string "[]" "" (when (string-match "\\(.*\\)" (car file-data))
;;                                                                     (match-string 0 (car file-data)))))            
;;             ;; get file type
;;             (if (string-match "\\(\\.hpp$\\)\\|\\(\\.hh$\\)\\|\\(\\.hxx$\\)\\|\\(\\.h$\\)" file-name)
;;               (setq file-type "h")
;;               (setq file-type "cpp"))

;;             (setq file-data (cdr file-data))

;;             (let ((tag-group nil)
;;                   (tag-text)
;;                   (tag-key)
;;                   (tag-index)
;;                   (break-cond-p nil))

;;               (while (and (not break-cond-p) file-data )
;;                 (setq file-line (car file-data))
;;                 ;; break condition
;;                 (if (eq (string-to-char file-line) ?\^L)
;;                     (setq break-cond-p t))

;;                 ;; parse file structure
               
;;                 (setq tag-text (car (split-string file-line ?\^?))) ;; tag-text
;;                 (setq file-line (cdr (split-string file-line ?\^?)))
;;                 (setq tag-key (car (split-string file-line ?\^A))) ;; tag-key
;;                 (setq tag-index (cdr (split-string file-line ?\^A))) ;; tag-index (row,diff); diff from begining of file

;;                 ;; const char features[] = {""features2,1
;;                 ;; -> typeCat:data dataTag rowIndex dataData
                
                
;;                 )))))
    
;;     ))

;; (message "%s" (when (string-match "\\(^\s*typedef\s+\\)" "typedef unsigned long ui32_t;") 
;;                 (match-string 0 "typedef unsigned long ui32_t;")))

  ;; (message "%s" (replace-regexp-in-string "[]" "" (when (string-match "\\(.*\\)" "bitset.hpp1,0") 
  ;;                                                       (match-string 0 "bitset.hpp1,0"))))

;; loop at string
;; (loop for char across "text" do (...))

(defun helm-complete-tag-get-by-name (expr)
  "list of candidates to complete by expression. expression can be variable or data type"
  )



(defun helm-complete-tag-create-index ()
  "Parse entries of TAGS file. Store result in helm-complete-tag-index."
  (let (
        ;; init
        (project-tag-file (if (fboundp 'projectile-project-root)
                              (concat (projectile-project-root) "TAGS")
                            nil))
        (project-tag-file-data nil))
    (if (boundp 'project-tag-file)
        (progn          
          ;; init tags table structure
          (setq helm-complete-tag-table nil)
          (setq project-tag-file-data (helm-complete-tag-read-file project-tag-file))
          (if (boundp 'project-tag-file-data)
              ;; generate internal struture for TAGS file
              (setq helm-complete-tag-index (helm-complete-tag-parse-index-data project-tag-file-data)))
          t)
      nil
      )
    )
  )


(message "%s" (helm-complete-tag-create-index))

(defun print-element-of-list (list)
  "Print list elements."
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun test-f ()
  "Test"
  (when (helm-complete-tag-create-index)
    (print-element-of-list helm-complete-tag-index)))

;; test
(test-f)
;; test async
(funcall'run-with-timer 0.01 nil
                              #'test-f)

(print-element-of-list (helm-complete-tag-create-index))

(message "%s" 
         (helm :sources (helm-build-sync-source "test"
                          :candidates '(one two three)
                          :fuzzy-match t)
               :buffer "*helm test*")
         )

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
;; for list support
(require 'dll)

;; Реализуемые механизмы
;; 1. поиск кандидатов
;;   * для завершения ввода переменной V, а так же типа данных T (слева от курсора выражение не разделенное пробелами)
;;   * для элемента типа T, связанного с переменной V (курсор следует за символами {'::';'.';'->'})
;;   * для объявления типа (в текущем положении курсора, если слева и справа от его позиция имеет значение пусто)
;; 2. выполнение переходов
;;   * h/cpp по имени тэга для типа данных (определение <-> реализация) 
;;   * [C-c p s a] для переменной ( В идеале, выводить индекс использования переменной внутри тела функции, для глобальных переменных, фильтр по типу и вывод аналогичный [C-c p s a])


(defvar helm-complete-tag-table
  "parsed TAGS file entries")


(defun helm-complete-tag-read-file (path)
  "read TAGS file as list of data"
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

;; helm-complete-tag-table structure
;;  (source:file path type={h,cpp}
;;      (namespace namespacetag-1 rowIndex
;;         (typeCat:type typeTag rowIndex typeData) 
;;         (typeCat:data dataTag rowIndex dataData) 
;;         (typeCat:struc strucTypeTag rowIndex
;;              (cat:def
;;                  (memCat:type strucMemberTag-1 rowIndex strucMemberData-1)
;;                  (memCat:data strucMemberTag-2 rowIndex strucMemberData-2)
;;                  (memCat:method strucMemberTag-3 rowIndex strucMemberData-3)
;;                  (memCat:friend strucMemberTag-4 rowIndex  strucMemberData-4))
;;              (cat:impl
;;                  (memcat:implData strucMemberTag-2 rowIndex strucMemberData-2)
;;                  (memcat:implMethod strucMembertag-3 rowIndex strucMemberData-3)))
;;          (typeCat:class classTypeTag rowIndex
;;              (cat:def
;;                  (memCat:type classMemberTag-1 rowIndex classMemberData-1)
;;                  (memCat:data classMemberTag-2 rowIndex classMemberData-2)
;;                  (memCat:method classMemberTag-3 rowIndex classMemberData-3)
;;                  (memCat:friend classMemberTag-4 rowIndex classMemberData-4))
;;              (cat:impl
;;                  (memcat:implData classMemberTag-2 rowIndex classMemberData-2)
;;                   (memcat:implMethod classMembertag-3 rowIndex classMemberData-3)))
;;          (typeCat:func funcTypeTag-1 rowIndex funcTypeData-1)
;;          (typecat:funcImpl funcTypeTag-1 rowIndex funcTypeData-1)
;;          (typeCat:macro macroTypeTag rowIndex macroTypeData)
;;          (namespace namespacetag-2 rowIndex
;;              (...)
;;          )
;;       )
;;   )

(defun helm-complete-tag-parse-data (file-data)
  "parse list of data from TAGS file. Generate helm-complete-tag-table structure"
  (let ((file-line nil)
        (file-path nil)
        (file-name nil)
        (file-type bil))
    (while file-data      
      (setq file-line (car file-data))
      ;;(message "char %s" (string-to-char item))
      (if (eq (string-to-char file-line) ?\^L)
          ;; new file - new structure
          (progn
            ;; get full path
            (setq file-data (cdr file-data))            
            (setq file-path (replace-regexp-in-string "," "" (when (string-match "\\(.*\\)," (car file-data))
                                                               (match-string 0 (car file-data)))))
            ;; get file name
            (setq file-data (cdr file-data))
            (setq file-name (replace-regexp-in-string "[]" "" (when (string-match "\\(.*\\)" (car file-data))
                                                                    (match-string 0 (car file-data)))))            
            ;; get file type
            (if (string-match "\\(\\.hpp$\\)\\|\\(\\.hh$\\)\\|\\(\\.hxx$\\)\\|\\(\\.h$\\)" file-name)
              (setq file-type "h")
              (setq file-type "cpp"))

            (setq file-data (cdr file-data))
            (while file-data
              ;; parse file structure
              ))))
      
    ))


  ;; (message "%s" (replace-regexp-in-string "[]" "" (when (string-match "\\(.*\\)" "bitset.hpp1,0") 
  ;;                                                       (match-string 0 "bitset.hpp1,0"))))

;; loop at string
;; (loop for char across "text" do (...))

(defun helm-complete-tag-get-by-name (expr)
  "list of candidates to complete by expression. expression can be variable or data type"
  )


(defun helm-complete-tag-parse-file ()
  "Parse entries of TAGS file. Store result in helm-complete-tag-table."
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
              (setq helm-complete-tag-table (helm-complete-tag-parse-data project-tag-file-data)))))
    )
  )

(helm-complete-tag-parse-file)

(message "%s" 
         (helm :sources (helm-build-sync-source "test"
                          :candidates '(one two three)
                          :fuzzy-match t)
               :buffer "*helm test*")
         )

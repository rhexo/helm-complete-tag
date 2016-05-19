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


;; Реализуемые механизмы
;; 1. поиск кандидатов
;;   * для завершения ввода переменной V, а так же типа данных T (слева от курсора выражение не разделенное пробелами)
;;   * для члена типа данных T, связанного с переменной V (курсор следует за символами {'::';'.';'->'})
;;   * для объявления типа (в текущем положении курсора, если слева и справа от его позиция имеет значение пусто)
;; 2. выполнение переходов
;;   * h/cpp по имени тэга для типа данных (определение <-> реализация) 
;;   * [C-c p s a] для переменной ( В идеале, выводить индекс использования переменной внутри тела функции, для глобальных переменных, фильтр по типу и вывод аналогичный [C-c p s a])

(message "%s" 
         (helm :sources (helm-build-sync-source "test"
                          :candidates '(one two three)
                          :fuzzy-match t)
               :buffer "*helm test*")
         )

;;; TEST-FUNCS --- Unit tests for the Piper `funcs'
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 12 May 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  ERT-based Unit Tests for the functions in the `funcs.el' file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(ert-deftest test-range-of-numbers ()
  "Validating that `range-of-numbers' correctly returns a list of numbers."
  (should (equal (range-of-numbers "1") '(1)))
  (should (equal (range-of-numbers "1, 3") '(1 3)))
  (should (equal (range-of-numbers "1 4") '(1 4)))
  (should (equal (range-of-numbers "1, 5 7") '(1 5 7))))

(ert-deftest test-range-of-numbers-with-range ()
  (should (equal (range-of-numbers "1, 3-5, 7") '(1 3 4 5 7)))
  (should (equal (range-of-numbers "1 3 - 5 7") '(1 3 4 5 7)))
  (should (equal (range-of-numbers "1-4") '(1 2 3 4))))

(ert-deftest test-range-of-numbers-empty ()
  (should (equal (range-of-numbers "") '()))
  (should (equal (range-of-numbers "  ") '())))

(ert-deftest test-range-of-numbers-initial-dash ()
  (should (equal (range-of-numbers "- 3") '(1 2 3)))
  (should (equal (range-of-numbers "-3") '(1 2 3)))
  (should (equal (range-of-numbers "-3, 5") '(1 2 3 5))))

(ert-deftest test-column-lines-for-line ()
  (let ((aline "first second third fourth fifth"))
    (should (equal (column-lines-for-line aline " " '(1)) '("first")))
    (should (equal (column-lines-for-line aline " " '(1 3)) '("first" "third")))
    (should (equal (column-lines-for-line aline " " '()) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test-funcs.el ends here

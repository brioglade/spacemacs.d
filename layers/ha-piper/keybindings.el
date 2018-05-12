;;; KEYBINDINGS --- Keys for the piper commands SPC x l
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 11 May 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Simple list of the keybindings that call various function in the piper
;;    group (see the `funcs.el' file). These are all under the Spacemacs leader
;;    sequence `x l' (to be next to the `sort-lines' function).
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

(spacemacs/set-leader-keys "x l !" 'pipe)
(spacemacs/set-leader-keys "x l f" 'spacemacs/flush-lines)
(spacemacs/set-leader-keys "x l k" 'spacemacs/keep-lines)
(spacemacs/set-leader-keys "x l ," 'column-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings.el ends here

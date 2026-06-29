;;; coc-dc.el --- A Clash of Clans damage calculator -*- lexical-binding: t; -*-

;; Copyright (C) 2024  S0mbr3

;; Author: S0mbr3 <0xf2f@proton.me>
;; Keywords: games
;; Homepage: https://github.com/S0mbr3/coc-damage-calculator
;; Version: 1
;; Package-Requires: ((emacs "27.2") (hydra "0.14.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Find optimal spell and equipment combos for building damage in Clash of Clans.


;;; Code:

(require 'hydra)

(defun coc-dc--earthquake-level-picker (level)
  "Pick the percentage for the earthquakes.

LEVEL is the level of the earthquakes."
  (let ((levels '(14.5 17 21 25 29 29 29 29)))
    (elt levels (1- level))))

(defun coc-dc--earthquake-damage-calculator (level number hp)
  "Calculate earthquakes damage.

LEVEL is the level of the earthquake.
NUMBER is how many earthquakes used.
HP is the building hp targeted."
  (let* (( i 2)
	 (damage
	  (/ (* hp (coc-dc--earthquake-level-picker level)) 100) )
	 (first damage)
	 (total first))
    (while (<= i number)
      (setq total (+ total (/ first (- (* 2 i) 1))))
      (setq i (1+ i)))
    total))

(defun coc-dc-earthquake-hp-building-left (level number hp)
  "Calculate the remaining building hp after earthquakes attacks.

LEVEL is the level of the earthquakes.
NUMBER is how many earthquakes used.
HP is the building hp targeted."
  (interactive "nEnter the level of the earthquake:\n\
nEnter the number of earthquakes:\n\
nEnter the hp of the building:")
  (print (format "The building has %d hp left"
		 (- hp(coc-dc--earthquake-damage-calculator level number hp)))))

(defun coc-dc--fireball-level-picker (level)
  "Pick the good damage for the fireball LEVEL."
  (let ((levels '(1500 1500 1700 1700 1800 1950 1950 2050 2200
		       2200 2350 2650 2650 2750 3100 3100 3250
		       3400 3400 3500 3650 3650 3750 3900 3900
		       3950 4100)))
    (elt levels (1- level))))


(defun coc-dc-fireball-hp-building-left (level hp)
  "Calculate the remaining buld hp after fireball attack.

LEVEL is the level of the fireball.
HP is the bulding hp targeted."
  (interactive "nEnter the level of the fireball: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--fireball-level-picker level)))))

(defun coc-dc-fireball-and-earthquake-calculator
    (fireball-level earthquake-level earthquake-number hp)
  "Calculate the remaining HP of a building after fireball and earthquake damage.

FIREBALL-LEVEL is the level of the fireball attack.
EARTHQUAKE-LEVEL is the level of the earthquake attack.
EARTHQUAKE-NUMBER is the number of consecutive earthquake attacks.
HP is the initial hit points of the building."
  (interactive "nEnter the level of the fireball: \n\
nEnter the level of the earthquake: \n\
nEnter the number of earthquakes: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--fireball-level-picker fireball-level )
		    (coc-dc--earthquake-damage-calculator
		     earthquake-level earthquake-number hp)))))

(defun coc-dc-fireball-and-arrow-calculator
    (fireball-level arrow-level  hp)
  "Calculate the remaining HP of a building after fireball and giant arrow damage.

FIREBALL-LEVEL is the level of the fireball.
ARROW-LEVEL is the level of giant arrow.
HP is the initial hit points of the building."
  (interactive "nEnter the level of the fireball: \n\
nEnter the level of the giant arrow: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--fireball-level-picker fireball-level)
		    (coc-dc--giant-arrow-level-picker arrow-level)))))

(defun coc-dc-fireball-earthquake-arrow-calculator
    (fireball-level earthquake-level earthquake-number arrow-level hp)
  "Calculate the remaining HP of a building after the following setup:
-fireball, giant arrow and earthquake.

FIREBALL-LEVEL is the level of the fireball.
ARROW-LEVEL is the level of giant arrow.
EARTHQUAKE-LEVEL is the level of the earthquake.
EARTHQUAKE-NUMBER is the level of the earthquake.
HP is the initial hit points of the building."
  (interactive "nEnter the level of the fireball: \n\
nEnter the level of the earthquake: \n\
nEnter the number of earthquakes: \n\
nEnter the level of the giant arrow: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--fireball-level-picker
			fireball-level)
		    (coc-dc--earthquake-damage-calculator
		     earthquake-level earthquake-number hp)
		    (coc-dc--giant-arrow-level-picker
		     arrow-level)))))

(defun coc-dc--rocket-spear-level-picker (level)
  "Pick the damage for the rocket spear.

LEVEL is the level of the rocket spear."
  (let ((levels '(350  350 420 420 420 490 490 490 560 560 560
		       630 630 630 700 700 700 770 770 770 840
		       840 840 910 910 910 980)))
    (elt levels (1- level))))

(defun coc-dc-rocket-hp-building-left (level hp)
  "Calculate the remaining hp of a building after a rocket spear attack.

LEVEL is the level of the rocket spear.
HP is the building hp targeted."
  (interactive "nEnter the level of the rocket spear: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d left"
		 (- hp (coc-dc--rocket-spear-level-picker level)))))

(defun coc-dc--lightning-level-picker (level)
  "Pick the percentage for the lightning spell.

LEVEL is the level of the lightning spell."
  (let ((levels '(150 180 210 240 270 320 400 480 560 600 640 680 720)))
    (elt levels (1- level))))

(defun coc-dc--lightning-damage-calculator (level number)
  "Calculate lightnings damage.

LEVEL is the level of the lightning spell.
NUMBER is how many lightnings used.
HP is the building hp targeted."
  (let* ((i 2)
	 (damage (coc-dc--lightning-level-picker level))
	 (total damage))
    (while (<= i number)
      (setq total (+ total damage))
      (setq i (1+ i)))
    total))

(defun coc-dc-lightning-hp-building-left (level number hp)
  "Calculate the remaining hp of a building after lightnings attacks.

LEVEL is the level of the lightning spells.
NUMBER is how many lightnings used.
HP is the building hp targeted."
  (interactive "nEnter the level of the lightning: \n\
nEnter the number of lightnings:  \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--lightning-damage-calculator level number)))))

(defun coc-dc--giant-arrow-level-picker (level)
  "Pick the damage for the giant arrow.

LEVEL is the level of the giant arrow."
  (let ((levels '(750 750 850 850 850 1000 1000 1000 1100 1100
		      1100 1200 1200 1200 1350 1350 1350 1500)))
    (elt levels (1- level))))

(defun coc-dc-giant-arrow-hp-building-left (level hp)
  "Calculate the remaining hp of a building after a giant arrow attack.

LEVEL is the level of the giant arrow.
HP is the building hp targeted."
  (interactive "nEnter the level of the giant arrow: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--giant-arrow-level-picker level)))))

(defun coc-dc--spiky-ball-level-picker (level)
  "Pick the damage for the spiky ball.

LEVEL is the level of the spiky ball."
  (let ((levels '(1000 1000 1250 1250 1250 1500 1500 1500 1750
		       1750 1750 2000 2000 2000 2250 2250 2250
		       2500 2500 2500 2750 2750 2750 3000 3000
		       3000 3250)))
    (elt levels (1- level))))

(defun coc-dc-spiky-ball-hp-building-left (level hp)
  "Calculate the remaining hp of a building after a spiky ball attack.

LEVEL is the level of the spiky ball.
HP is the building hp targeted."
  (interactive "nEnter the level of the spiky ball: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--spiky-ball-level-picker level)))))

(defun coc-dc--rocket-backpack-level-picker (level)
  "Pick the good damage for the rocket backpack LEVEL."
  (let ((levels '(575 575 750 750 750 950 950 950 1125
		       1125 1125 1325 1325 1325 1500 1500 1500
		       1700 1700 1700 1875 1875 1875 2050 2050
		       2050 2150)))
    (elt levels (1- level))))


(defun coc-dc-rocket-backpack-hp-building-left (level hp)
  "Calculate the remaining buld hp after rocket backpack attack.

LEVEL is the level of the rocket backpack.
HP is the bulding hp targeted."
  (interactive "nEnter the level of the rocket backpack: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--rocket-backpack-level-picker level)))))


(defun coc-dc-rocket-backpack-earthquake-arrow-calculator
    (rocket-backpack-level earthquake-level earthquake-number arrow-level hp)
  "Calculate the remaining HP of a building after the following setup:
-rocket backpack, giant arrow and earthquake.

ROCKET-BACKPACK-LEVEL is the level of the rocket backpack.
ARROW-LEVEL is the level of giant arrow.
EARTHQUAKE-LEVEL is the level of the earthquake.
EARTHQUAKE-NUMBER is the level of the earthquake.
HP is the initial hit points of the building."
  (interactive "nEnter the level of the rocket backpack: \n\
nEnter the level of the earthquake: \n\
nEnter the number of earthquakes: \n\
nEnter the level of the giant arrow: \n\
nEnter the hp of the building: ")
  (print (format "The building has %d hp left"
		 (- hp (coc-dc--rocket-backpack-level-picker
			rocket-backpack-level)
		    (coc-dc--earthquake-damage-calculator
		     earthquake-level earthquake-number hp)
		    (coc-dc--giant-arrow-level-picker
		     arrow-level)))))

(defun coc-dc-custom-hp-building-left (input-string)
  "Calculate the hp left of a building after a custom setup.

INPUT-STRING is the custom setup used to calculate the damage.

Very useful if you mix different spells levels like earthquakes.

e: earthquake
l: lightning
f: fireball
a: giant arrow
s: rocket spear
b: spiky ball
r: rocket backpack

EG:'eelfa' will apply:
- 2 sets of earthquakes one lightning set a fireball and a giant arrow."
  (interactive "sEnter the custom setup: ")
  (let ((string-length (length input-string))
	(damage 0)
	(hp (read-number "The hp of the building: ")))
    (dotimes (i string-length)
      (let ((char (aref input-string i)))
        (cond
         ((eq char ?f)
	  (setq damage
		(+ damage (coc-dc--fireball-level-picker
			   (read-number "Level of the fireball: ")))))
         ((eq char ?a)
	  (setq damage
		(+ damage (coc-dc--giant-arrow-level-picker
			   (read-number "Level of the giant arrow: ")))))
         ((eq char ?s)
	  (setq damage
		(+ damage (coc-dc--rocket-spear-level-picker
			   (read-number "Level of the rocket spear: ")))))
         ((eq char ?b)
	  (setq damage
		(+ damage (coc-dc--spiky-ball-level-picker
			   (read-number "Level of the spiky ball: ")))))
         ((eq char ?r)
	  (setq damage
		(+ damage (coc-dc--rocket-backpack-level-picker
			   (read-number "Level of the rocket backpack: ")))))
         ((eq char ?e)
	  (setq damage
		(+ damage (coc-dc--earthquake-damage-calculator
			   (read-number "Level of the earthquake: ")
			   (read-number "Number of earthquakes: ")
			   hp))))
         ((eq char ?l)
	  (setq damage
		(+ damage (coc-dc--lightning-damage-calculator
			   (read-number "Level of the lightning: ")
			   (read-number "Number of lightnings: ")))))
         (t (message "Other character '%c' at index %d" char i)))))
    (print (format "The building has %d hp left" (- hp damage)))))

(defface coc-dc-hydra-title-face
  '((t (:foreground "#FFA500" :weight bold :height 1.2)))
  "Face for hydra titles.")

(defface coc-dc-hydra-command-face
  '((t (:foreground "#87CEEB")))
  "Face for hydra commands.")

;;;###autoload
(defun coc-dc-menu()
  "Open coc-dc menu with hydra."
  (interactive)
  (unless (fboundp 'coc-hydra/body)
    (eval `(defhydra coc-hydra (:color teal :hint nil)
	     "
  ██████╗ ██████╗  ██████╗    ██████╗        ██████╗
  ██╔════╝██╔═══██╗██╔════╝    ██╔══██╗      ██╔════╝
  ██║     ██║   ██║██║         ██║  ██║█████╗██║
  ██║     ██║   ██║██║         ██║  ██║╚════╝██║
  ╚██████╗╚██████╔╝╚██████╗    ██████╔╝      ╚██████╗
  ╚═════╝ ╚═════╝  ╚═════╝    ╚═════╝        ╚═════╝
  ^^^
  ^Equipements^      ^Spells^         ^Combined^
  ^^^^^^^^-------------------------------------------------------
  _h_: ^^^^Fireball      _b_: ^^^^Earthquake  _w_: ^^^^Fireball and Earthquake
  _j_: ^^^^Giant arrow   _k_: ^^^^Lightning   _d_: ^^^^Fireball Earthquake Giant arrow
  _s_: ^^^^Rocket spear                 _c_: ^^^^Fireball And Giant arrow
  _l_: ^^^^Spiky ball                  _f_: ^^^^Custom setup
  _r_: ^^^^Rocket backpack              _x_: ^^^^Rocket backpack Earthquake Giant arrow
  "
	     ;; Equipements
	     ("h" coc-dc-fireball-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ("j" coc-dc-giant-arrow-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ("s" coc-dc-rocket-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ("l" coc-dc-spiky-ball-hp-building-left
	      :face coc-dc-hydra-command-face)

             ("r" coc-dc-rocket-backpack-hp-building-left
	      :face coc-dc-hydra-command-face)


	     ;; Spells
	     ("b" coc-dc-earthquake-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ("k" coc-dc-lightning-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ;; Combined
	     ("w" coc-dc-fireball-and-earthquake-calculator
	      :face coc-dc-hydra-command-face)

	     ("d" coc-dc-fireball-earthquake-arrow-calculator
	      :face coc-dc-hydra-command-face)

	     ("c" coc-dc-fireball-and-arrow-calculator
	      :face coc-dc-hydra-command-face)

	     ("f" coc-dc-custom-hp-building-left
	      :face coc-dc-hydra-command-face)

	     ("x" coc-dc-rocket-backpack-earthquake-arrow-calculator
	      :face coc-dc-hydra-command-face)

	     ;; Quit
	     ("q" nil "quit" :color blue
	      :face coc-dc-hydra-command-face))))
  (when (fboundp 'coc-hydra/body) (coc-hydra/body)))

(provide 'coc-dc)

;;; coc-dc.el ends here

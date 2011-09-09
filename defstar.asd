;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80
;;;;
;;;;    This file is part of DEFSTAR, by Paul Sexton
;;;;    Released under the Gnu Public License version 3
;;;;
;;;;    DEFSTAR is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    DEFSTAR is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with DEFSTAR.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(defpackage #:defstar-system
  (:use :cl :asdf))

(in-package :defstar-system)

(defsystem defstar
    :name "defstar"
    :version "1.0.0"
    :author "eeeickythump@gmail.com"
    :components ((:file "defstar")))





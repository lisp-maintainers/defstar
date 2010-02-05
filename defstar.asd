;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:defstar-system
  (:use :cl :asdf))

(in-package :defstar-system)

(defsystem defstar
    :name "defstar"
    :version "0.0.0"
    :author "eeeickythump@gmail.com"
    :components ((:file "defstar")))





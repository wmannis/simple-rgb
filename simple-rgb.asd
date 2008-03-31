;;; -*- mode: lisp; syntax: common-lisp; encoding: utf-8 -*-
;;;
;;; $Id: simple-rgb.asd,v 1.1 2008-03-30 16:39:37-05 annis Exp annis $
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;; This is free software; you can redistribute it and/or modify it
;;; under the same terms as Perl (the Artistic Licence). 

(in-package :asdf)

(defsystem :simple-rgb
  :name "SIMPLE-RGB"
  :author "William S. Annis <wm.annis@gmail.com>"
  :version "0.01"
  :maintainer "William S. Annis <wm.annis@gmail.com>"
  :licence "Artistic License"
  :description "simple manipulation of {X|HT}ML RGB values"

  :components ((:file "package")
               (:file "rgb" :depends-on ("package"))))

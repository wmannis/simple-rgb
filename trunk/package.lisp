;;; -*- mode: lisp; syntax: common-lisp; encoding: utf-8 -*-
;;; Author: William S. Annis
;;; Version: $Revision: 1.1 $
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;; This is free software; you can redistribute it and/or modify it
;;; under the same terms as Perl (the Artistic Licence).

(defpackage :simple-rgb
  (:use :common-lisp)
  (:nicknames :rgb)
  (:export :rgb
           :rgb=
           :hsv
           :hsv-type-error
           :mix-rgb
           :mix-rgb!
           :greyscale-rgb
           :lighten-rgb
           :lighten-rgb!
           :darken-rgb
           :darken-rgb!
           :invert-rgb
           :compliment-rgb
           :contrast-rgb
           :xmlify-rgb
           :rgb->hsv
           :hsv->rgb
           :rotate-hsv
           :rotate-rgb))


;;; package.lisp ends here


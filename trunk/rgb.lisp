;;; -*- mode: lisp; syntax: common-lisp; package: simple-rgb encoding: utf-8 -*-
;;; Author: William S. Annis
;;; Version: $Revision: 1.5 $
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.


;;; Description: simple manipulation of 24-bit RGB color values.
;;; The colors are are simple vectors of the form #(R G B), with
;;; 0 <= R,G,B <= 255.

(in-package :simple-rgb)

(deftype rgb ()
  '(vector (unsigned-byte 8) 3))

(defun rgb (r g b)
  (make-array '(3) :element-type '(unsigned-byte 8)
                   :initial-contents (list r g b)))

(defun rgb= (a b)
  (declare (type rgb a b))
  (and (= (aref a 0) (aref b 0))
       (= (aref a 1) (aref b 1))
       (= (aref a 2) (aref b 2))))


(deftype hsv ()
  '(vector (float 0.0e0 1.0e0) 3))

(define-condition hsv-type-error (type-error)
  ((bugged-vector :initarg :bugged :accessor bugged-hsv-vector))
  (:report (lambda (condition stream)
             (format stream
                 "all elements of HSV must be floats between 0.0 and 1.0: ~A"
                 (bugged-hsv-vector condition)))))

(defun hsv (h s v)
  ;; The :ELEMENT-TYPE option in MAKE-ARRAY will only raise a condition on
  ;; type oddities if you try to insert a float that wants too much space
  ;; (CLHS 15.1.2.1).
  (unless (and (typep h '(float 0.0e0 1.0e0))
               (typep s '(float 0.0e0 1.0e0))
               (typep v '(float 0.0e0 1.0e0)))               
    (error (make-condition 'hsv-type-error :bugged (vector h s v))))
  (make-array '(3) :element-type '(float 0.0e0 1.0e0)
                   :initial-contents (list h s v)))


(defparameter +rgb-black+ (rgb 0 0 0))
(defparameter +rgb-white+ (rgb 255 255 255))

;;; ALPHA weights the mix, 0.0 favoring the first color, 1.0 the second.
(defun mix-rgb (a b &key (alpha 0.5))
  (declare (type (float 0 1.0) alpha)
           (type rgb a b))
  (let ((c (rgb 0 0 0)))
    (dotimes (i 3 c)
      (setf (aref c i)
            (round (+ (aref a i)
                      (* alpha
                         (- (aref b i)
                            (aref a i)))))))))

;;; This one overwrites the first argument with the mixed color.
(defun mix-rgb! (a b &key (alpha 0.5))
  (declare (type (float 0 1.0) alpha)
           (type rgb a b))
  (dotimes (i 3 a)
    (setf (aref a i)
          (round (+ (aref a i)
                    (* alpha
                       (- (aref b i)
                          (aref a i))))))))

;;; http://en.wikipedia.org/wiki/Grayscale
(defun greyscale-rgb (a)
  (declare (type rgb a))
  (let ((gs (round (+ (* .3 (aref a 0))
                      (* .59 (aref a 1))
                      (* .11 (aref a 2))))))
    (rgb gs gs gs)))

(defun lighten-rgb (a)
  (mix-rgb a +rgb-white+))

(defun lighten-rgb! (a)
  (mix-rgb! a +rgb-white+))

(defun darken-rgb (a)
  (mix-rgb a +rgb-black+))

(defun darken-rgb! (a)
  (mix-rgb! a +rgb-black+))

(defun invert-rgb (a)
  (rgb (- 255 (aref a 0))
       (- 255 (aref a 1))
       (- 255 (aref a 2))))

;;; http://livedocs.adobe.com/en_US/Illustrator/13.0/help.html?content=WS714a382cdf7d304e7e07d0100196cbc5f-6288.html
;;; This does nothing interesting to greys.
(defun compliment-rgb (a)
  (declare (type rgb a))
  (let* ((r (aref a 0))
         (g (aref a 1))
         (b (aref a 2))
         (min+max (+ (min r g b) (max r g b))))
    (rgb (- min+max r)
         (- min+max g)
         (- min+max b))))

(defun contrast-rgb (a &optional (cut 0.5))
  (declare (type rgb a))
  (let ((cutoff (round (* cut 255))))
    (labels ((contrastify (color-component)
               (if (>= cutoff color-component) 0 255)))
      (rgb (contrastify (aref a 0))
           (contrastify (aref a 1))
           (contrastify (aref a 2))))))

(defun xmlify-rgb (a)
  (declare (type rgb a))
  (format nil "#~2,'0X~2,'0X~2,'0X" (aref a 0) (aref a 1) (aref a 2)))


(defun rgb->hsv (a)
  (declare (type rgb a))
  (let* ((r (/ (aref a 0) 255.0))
         (g (/ (aref a 1) 255.0))
         (b (/ (aref a 2) 255.0))
         (max (max r g b))
         (min (min r g b))
         (v max))
    (if (= max min)
        (hsv 0.0 0.0 v)
        (let ((s (/ (- max min) max))
              (h 0))
          (cond ((= r max)
                 (setf h (- (/ (- max b) (- max min))
                            (/ (- max g) (- max min)))))
                ((= g max)
                 (setf h (+ 2.0 (- (/ (- max r) (- max min))
                                   (/ (- max b) (- max min))))))
                (t (setf h (+ 4.0 (- (/ (- max g) (- max min))
                                     (/ (- max r) (- max min)))))))
          (setf h (mod (/ h 6.0) 1))
          (hsv h s v)))))

(defun hsv->rgb (a)
  (declare (type hsv a))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2)))
    (labels ((rgb-from-floats (r g b)
               (apply #'rgb (mapcar #'(lambda (c)
                                        (coerce (round (* c 255)) 'integer))
                                    (list r g b)))))
      (if (= s 0.0)
          (rgb-from-floats v v v)
          (multiple-value-bind (i f) (truncate (* h 6.0))
            (let* ((p (* v (- 1.0 s)))
                   (q (* v (- 1.0 (* s f))))
                   (tv (* v (- 1.0 (* s (- 1.0 f))))))
              (cond ((= (mod i 6) 0) (rgb-from-floats v tv p))
                    ((= i 1) (rgb-from-floats q v p))
                    ((= i 2) (rgb-from-floats p v tv))
                    ((= i 3) (rgb-from-floats p q v))
                    ((= i 4) (rgb-from-floats tv p v))
                    ((= i 5) (rgb-from-floats v p q)))))))))

;;; (hsv->rgb (rotate-hsv (rgb->hsv color) 180)) == (compliment-rgb color)
(defun rotate-hsv (a rotation)
  (declare (type hsv a))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2))
        (scaled-rotation (/ rotation 360.0)))
    (hsv (mod (+ h scaled-rotation) 1.0) s v)))

(defun rotate-rgb (a rotation)
  (hsv->rgb (rotate-hsv (rgb->hsv a) rotation)))

;;; rgb.lisp ends here

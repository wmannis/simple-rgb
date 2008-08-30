;;; $Id$
;;;
;;; For now this is just for testing during development, and isn't
;;; loadable via ASDF trickery.

(asdf:oos 'asdf:load-op :simple-rgb)
(asdf:oos 'asdf:load-op :lift)
(use-package :simple-rgb)
(use-package :lift)


(deftestsuite rgb-tests () ())

(addtest (rgb-tests)
  types-rgb-reject-float
  (ensure-condition type-error   ; condition TYPE-ERROR required by spec
    (rgb 1.0 33 55)))

(addtest (rgb-tests)
  types-rgb-reject-range-0
  (ensure-condition type-error
    (rgb 400 33 55)))

(addtest (rgb-tests)
  types-rgb-reject-range-1
  (ensure-condition type-error
    (rgb -15 33 55)))

(addtest (rgb-tests)
  types-hsv-reject-non-float
  (ensure-condition hsv-type-error
    (hsv 0 0.3 0.6)))

(addtest (rgb-tests)
  types-hsv-reject-range-0
  (ensure-condition hsv-type-error
    (hsv 0.2 0.5 1.33)))

(addtest (rgb-tests)
  types-hsv-reject-range-1
  (ensure-condition hsv-type-error
    (hsv 0.2 -0.5 0.43)))

(addtest (rgb-tests)
  500-conversions
  (ensure (dotimes (i 500 t)
            (let* ((r (random 255))
                   (g (random 255))
                   (b (random 255))
                   (rgb (rgb r g b))
                   (converted-rgb (hsv->rgb (rgb->hsv rgb))))
              (unless (rgb= rgb converted-rgb)
                (return nil))))
          :report "at least one randomized HSV/RGB conversion failed"))

(addtest (rgb-tests)
  contrast-0
  (ensure-same (contrast-rgb (rgb 200 100 155) 0.6)
               (rgb 255 0 255)
               :test #'rgb=))

(addtest (rgb-tests)
  contrast-1
  (ensure-same (contrast-rgb (rgb 50 90 155))
               (rgb 0 0 255)
               :test #'rgb=))

(addtest (rgb-tests)
  greyscale-0
  (ensure-same (greyscale-rgb (rgb 55 55 55))
               (rgb 55 55 55)
               :test #'rgb=))

(addtest (rgb-tests)
  greyscale-1
  (ensure-same (greyscale-rgb (rgb 0 255 0))
               (rgb 150 150 150)
               :test #'rgb=))

(addtest (rgb-tests)
  greyscale-2
  (ensure-same (greyscale-rgb (rgb 150 255 0))
               (rgb 195 195 195)
               :test #'rgb=))

(addtest (rgb-tests)
  lighten-0
  (ensure-same (lighten-rgb (rgb 255 150 50))
               (rgb 255 202 152)
               :test #'rgb=))

(addtest (rgb-tests)
  lighten-mix-identical
  (ensure-same (lighten-rgb (rgb 255 150 50))
               (mix-rgb (rgb 255 150 50) simple-rgb::+rgb-white+)
               :test #'rgb=))

(addtest (rgb-tests)
  mix-rgb-0
  (ensure-same (mix-rgb (rgb 255 150 50) simple-rgb::+rgb-white+ :alpha .7)
               (rgb 255 224 194)
               :test #'rgb=))

(addtest (rgb-tests)
  mix-rgb-1
  (ensure-same (mix-rgb simple-rgb::+rgb-black+ simple-rgb::+rgb-white+)
               (rgb 128 128 128)
               :test #'rgb=))

(addtest (rgb-tests)
  complement-0
  (ensure-same (complement-rgb (rgb 55 72 33))
               (hsv->rgb (rotate-hsv (rgb->hsv (rgb 55 72 33)) 180))
               :test #'rgb=))

(addtest (rgb-tests)
  complement-1
  (ensure-same (complement-rgb (rgb 0 255 255))
               (rgb 255 0 0)
               :test #'rgb=))

(addtest (rgb-tests)
  hsv-rotation-0
  (ensure-same (hsv->rgb (rotate-hsv (rgb->hsv (rgb 255 0 0)) 120))
               (rgb 0 255 0)
               :test #'rgb=))

(addtest (rgb-tests)
  hsv-rotation-1
  (ensure-same (rotate-rgb (rgb 255 128 0) 120)
               (rgb 0 255 128)
               :test #'rgb=))


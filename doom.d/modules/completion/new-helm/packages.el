;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

;; Version 3.8.7 2022-08-03
;;(package! helm :pin "4ede199d5d1b7050486a0fdeecbbbf49fef31118")
;; Version 3.9.x 2023-05-29
(package! helm :pin "e56d0a82a4037ab11e75a89999796a809f51901c")
;; Version 3.9.x 2023-03-19
;;(package! helm :pin "f1ef8689514b20cd2c2a6c6be987d2c40a8f4a09")
;; Version 3.9.x 2022-11-29
;;(package! helm :pin "115847b8d5f4175d7469f873d50a7e41710db5aa")
;; Version 3.9.0 2022-11-12
;;(package! helm :pin "371df21b1e73be03b90028e2f5d8554f70a989b7")
;; Version 2021-03-30
(package! helm-c-yasnippet :pin "e214eec8b2875d8a7cd09006dfb6a8e15e9e4079")
;; Version v0.2.5 2019-08-12
(package! helm-company :pin "6eb5c2d730a60e394e005b47c1db018697094dde")
;; Version 2019-05-01
(package! helm-descbinds :pin "b72515982396b6e336ad7beb6767e95a80fca192")
;; Version 2016-02-12
;; Note: The github repo has a later date (2017), but that is only an insignificant documentation change. No change to .el file.
(package! helm-describe-modes :pin "11fb36af119b784539d31c6160002de1957408aa")
;; Version 2022-08-07
;;(package! helm-projectile :pin "6dcc543815984f7f40e99050b1ee3b68a088e160")
;; Version 2022-12-15
(package! helm-projectile :pin "35a2111d00c0c0c9d8743280d3f1243bb217118a")
;; Version 2022-08-20
;;(package! helm-projectile :pin "5813f7286533990783546c9c39c184faa034d1f1")
;; Version 2020-07-21
(package! helm-rg :pin "ee0a3c09da0c843715344919400ab0a0190cc9dc")
;; Version 2018-01-31
(package! swiper-helm :pin "93fb6db87bc6a5967898b5fd3286954cc72a0008")

(when (modulep! +childframe)
  ;; Version 2021-11-03
  (package! helm-posframe :pin "87461b52b6f3f378c63642a33f584d4a4ba28351"))

(when (modulep! +fuzzy)
  ;; Version 2022-04-02
  ;;(package! helm-flx :pin "27dd9e3ce385a3ca15092150e65781de14b5b00b")
  ;; Version 2022-10-20
  (package! helm-flx :pin "5220099e695a3586dba2d59640217fe378e66310")
  )

(when (modulep! +icons)
  ;; Version 2023-05-06
  (package! helm-icons :pin "dfefdb41c63217a1d6f57d4c8761b68f3def1a31")
  ;; Version 2021-03-30
  ;;(package! helm-icons :pin "8d2f5e705c8b78a390677cf242024739c932fc95")
  )

(when (modulep! :lang org)
  ;; Version 2021-03-24
  (package! helm-org :pin "d67186d3a64e610c03a5f3d583488f018fb032e4"))

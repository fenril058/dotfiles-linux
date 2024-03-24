;; -*- lexical-binding: t; -*-
;; GC Thresholdを512 MBに (最後に 100 MBにする)
;; Magic File Name を一時的に無効に (最後にもとに戻す)
(setq gc-cons-threshold (* 512 1024 1024))
(defconst default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 100 1024 1024)) ; default 800000
            ))

(setq read-process-output-max (* 1024 1024)) ; default 4096

(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8)
  (setq native-comp-speed 2))

(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(when (>= emacs-major-version 28)
  (setq native-comp-async-report-warnings-errors 'silent))

(provide 'early-init)
;;; early-init.el ends here

;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;;

(package! org-jira)

(package! page-break-lines)

(package! google-c-style
  :recipe (:type git :host github :repo "google/styleguide" :branch "gh-pages"))

(unless (featurep! :editor evil)
  (package! iedit))

;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; ui fine-tuning
(setq doom-theme 'doom-opera)

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 14)
;;       doom-variable-pitch-font (font-spec :family "Noto Sans CJK KR" :size 14)
;;       doom-unicode-font (font-spec :family "JetBrains Mono")
;;       doom-big-font (font-spec :family "JetBrains Mono" :size 24))
;; (set-fontset-font t 'hangul (font-spec :name "Noto Sans Mono CJK KR"))
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Noto Sans CJK KR" :size 14)
      doom-unicode-font (font-spec :name "Noto Sans Mono CJK KR"))

;; Korean input methods
(setq default-input-method "korean-hangul")
(map! "S-SPC" #'toggle-input-method)


;; I don't like line numbers
(setq display-line-numbers-type nil)

;; org mode
(setq org-directory "~/Dropbox/Org")
(after! org
  (setq org-tags-column -100))

;; deft
(when (featurep! :ui deft)
  (after! deft
    (setq deft-directory org-directory)))

;; fill column mode not in org mode
(remove-hook! '(text-mode-hook) #'+fill-column-enable-h)

;; fill-column = 100
(setq-default fill-column 100)

;; lsp config not to see warning every time
(when (featurep! :tools lsp)
  (setq lsp-file-watch-threshold 10000))

;; word-wrap config
(when (featurep! :editor word-wrap)
  (add-hook! (c-mode-common
              python-mode
              emacs-lisp-mode) #'+word-wrap-mode))

;; rgb mode config
(when (featurep! :tools rgb)
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; additional packages
(use-package! page-break-lines
  :hook ((text-mode
          emacs-lisp-mode
          lisp-mode
          compilation-mode
          outline-mode
          help-mode) . page-break-lines-mode))

;; google-c-style
(use-package! google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; projectile configs
(after! projectile
  (projectile-register-project-type 'cmake
                                    '("CMakeLists.txt")
                                    :compilation-dir "build"
                                    :configure "cmake %s"
                                    :compile "cmake --build . -- -j8"
                                    :test "ctest")
  (projectile-register-project-type 'waf
                                    '("wscript")
                                    :configure "waf distclean configure"
                                    :compile "waf")

  (define-key!
    [remap projectile-compile-project] nil))

;; ccls set out-of-tree build directory
(after! ccls
  (setq ccls-initialization-options
        (append ccls-initialization-options
                `(:compilationDatabaseDirectory "build"))))

;; iedit only for Emacs key-bindings
(use-package! iedit
  :unless (featurep! :editor evil)
  :bind (("C-;" . iedit-mode)
         ("C-:" . iedit-mode-toggle-on-function))
  :config (set-face-inverse-video 'iedit-occurrence t))

;; emacs additional key-bindings
(unless (featurep! :editor evil)
  (map!
   ;; dired-jump binding is not working before 'dired' is first used
   "C-x k"   #'doom/kill-this-buffer-in-all-windows
   "C-x C-j" #'dired-jump

   ;; projectile-comfile-command should be used instead of ivy/project-compile
   [remap projectile-compile-project]  nil

   (:when (featurep! :completion ivy)
    "C-s" #'counsel-grep-or-swiper
    "C-r" #'counsel-grep-or-swiper-backward)

   (:when (featurep! :tools lsp)
    "C-c C-f"  #'+default/lsp-format-region-or-buffer) ;; this is my coding habit

   (:when (featurep! :editor multiple-cursors)
    "M-3" #'mc/mark-previous-like-this
    "M-4" #'mc/mark-next-like-this
    "M-#" #'mc/skip-to-previous-like-this
    "M-$" #'mc/skip-to-next-like-this)
   )
  )

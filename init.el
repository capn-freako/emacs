(require 'package)
(setq package-enable-at-startup nil)

;; https://emacs.stackexchange.com/a/2989
(setq package-archives
      '(("elpa"         . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("elpa"         . 5)
        ("gnu"          . 2)
        ("melpa"        . 0)))

(package-initialize)
(package-refresh-contents)

;; From Conal's config.:
;; List from package-activated-list
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(let ((package-list
       '(attrap dante flycheck-haskell flycheck haskell-mode
	 markdown-mode pkg-info use-package bind-key
	 ;; intero company lcr dash
	 ;; define-word elisp-slime-nav exec-path-from-shell f 
         ;; mmm-mode nlinum epl popwin s seq
	 ;; w3m yaml-mode zoom-frm frame-cmds 
         ;; frame-functions
         )))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; Bootstrap `use-package`
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (add-to-list 'load-path "~/.emacs.d/elpa/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/elpa/use-package/"))

(savehist-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-use-presentation-mode t)
 '(haskell-tags-on-save t)
 '(markdown-command "pandoc")
 '(package-selected-packages
   (quote
    (magit use-package dante haskell-emacs markdown-mode haskell-mode)))
 '(save-place t)
 '(show-paren-mode t)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS SRC Bzr Hg Mtn))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "SteelBlue3"))))
 '(font-lock-doc-face ((t (:foreground "SteelBlue2"))))
 '(font-lock-function-name-face ((t (:foreground "MediumOrchid2"))))
 '(font-lock-keyword-face ((t (:foreground "MediumPurple1"))))
 '(font-lock-string-face ((t (:foreground "dark gray"))))
 '(font-lock-type-face ((t (:foreground "DarkSlateGray1"))))
 '(font-lock-variable-name-face ((t (:foreground "MistyRose1")))))
;; (setq mac-command-modifier 'meta)
;; initial window settings
(setq initial-frame-alist
      '((width . 92)
        (height . 54)
	(font . "Menlo-14")
        (background-color . "gray10")
        (foreground-color . "gray90")))
;; subsequent window settings
(setq default-frame-alist
      '((menu-bar-lines . 1)
        (tool-bar-lines . 0)
        (width . 92)
        (height . 52)
	(font . "Menlo-14")
        (background-color . "gray10")
        (foreground-color . "gray90")))

;; haskell-mode stuff
(require 'haskell-interactive-mode)
(require 'haskell-process)
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
;; (setq haskell-compile-cabal-build-command "stack build")

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(eval-after-load "haskell"
  '(progn
     (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)     
     (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
     (define-key interactive-haskell-mode-map (kbd "M-n")     'next-error)
     (define-key interactive-haskell-mode-map (kbd "M-p")     'previous-error)
     (define-key interactive-haskell-mode-map (kbd "C-c M-p") 'first-error)
;;      (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;      (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;      (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;      (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;      (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;      (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;      (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;      (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;      (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;      (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;      (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
   )
)
(with-eval-after-load 'haskell-mode
 (setq haskell-process-args-ghci
       '("-ferror-spans" "-fshow-loaded-modules"))
 (setq haskell-process-args-cabal-repl
       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
 (setq haskell-process-args-stack-ghci
       '("--ghci-options=-ferror-spans -fshow-loaded-modules"
         "--no-build" "--no-load"))
 (setq haskell-process-args-cabal-new-repl
       '("--ghc-options=-ferror-spans -fshow-loaded-modules")))
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  )
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)
(add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint))))
;; markdown-mode stuff
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

;; John Wiegley's prescription for automatic `git-monitor`ing upon `magit-status`ing:
(defun magit-monitor (&optional no-display)
  "Start git-monitor in the current directory."
  (interactive)
  (let* ((path (file-truename
                (directory-file-name
                 (expand-file-name default-directory))))
         (name (format "*git-monitor: %s*"
                       (file-name-nondirectory path))))
    (unless (and (get-buffer name)
                 (with-current-buffer (get-buffer name)
                   (string= path (directory-file-name default-directory))))
      (with-current-buffer (get-buffer-create name)
        (cd path)
        (ignore-errors
          (start-process "*git-monitor*" (current-buffer)
                         "git-monitor" "-d" path))))))

(add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

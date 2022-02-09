;;; dbanas-init --- David Banas' Emacs main configuration file.
;;; Commentary:

(require 'package)

;;; Code:

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
     exec-path-from-shell magit
     pos-tip popup button-lock flycheck-color-mode-line flycheck-liquidhs
     ;; intero company lcr dash
     ;; define-word elisp-slime-nav
     ;; f
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
 '(column-number-mode t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(exec-path-from-shell-variables
   (quote
    ("PATH" "MANPATH" "PYTHONPATH" "PKG_CONFIG_PATH" "DYLD_FALLBACK_LIBRARY_PATH")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-use-presentation-mode t)
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(load-prefer-newer t)
 '(markdown-command "pandoc")
 '(package-selected-packages
   (quote
    (magit use-package dante haskell-emacs markdown-mode haskell-mode)))
 '(save-place t)
 '(show-paren-mode t)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS SRC Bzr Hg Mtn)))
 '(verilog-auto-newline nil)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-directive 2)
 '(verilog-indent-level-module 2)
 '(verilog-indent-lists nil))

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
    (font . "Mononoki-18")
        (background-color . "gray10")
        (foreground-color . "gray90")))
;; subsequent window settings
(setq default-frame-alist
      '((menu-bar-lines . 1)
        (tool-bar-lines . 0)
        (width . 92)
        (height . 52)
    (font . "Mononoki-18")
        (background-color . "gray10")
        (foreground-color . "gray90")))

;; ---------------- haskell-mode stuff --------------------------
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'flycheck)
(require 'flycheck-liquidhs)

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

;; (with-eval-after-load 'haskell-mode
;;  (setq haskell-process-args-ghci
;;        '("-ferror-spans" "-fshow-loaded-modules"))
;;  (setq haskell-process-args-cabal-repl
;;        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
;;  (setq haskell-process-args-stack-ghci
;;        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
;;          "--no-build" "--no-load"))
;;  (setq haskell-process-args-cabal-new-repl
;;        '("--ghc-options=-ferror-spans -fshow-loaded-modules")))

(with-eval-after-load 'haskell-mode
 (setq haskell-process-args-ghci
       '("-ferror-spans"))
 (setq haskell-process-args-cabal-repl
       '("--ghc-options=-ferror-spans"))
 (setq haskell-process-args-stack-ghci
       '("--ghci-options=-ferror-spans"
         "--no-build" "--no-load"))
 (setq haskell-process-args-cabal-new-repl
       '("--ghc-options=-ferror-spans")))

;; --------------- Dante ------------------
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  ;; OR:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  )

(add-hook 'dante-mode-hook
  '(lambda ()
     (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
   )
)
;; ----------------------------------------

;; ----------------------- Configure Flycheck ------------------


;; Global Flycheck
(global-flycheck-mode)

;; Rerun check on idle and save
(setq flycheck-check-syntax-automatically '(mode-enabled idle-change save))

;; (setq flymake-no-changes-timeout nil)
;; (setq flymake-start-syntax-check-on-newline nil)
;; (setq flycheck-check-syntax-automatically '(save mode-enabled))

;; (add-hook 'haskell-mode-hook          'flycheck-mode)
;; (add-hook 'literate-haskell-mode-hook 'flycheck-mode)

;; (require 'flycheck-color-mode-line)

;; (eval-after-load "flycheck"
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; (set-face-attribute 'flycheck-error nil
;;                     :foreground "red"
;; 	            :background "pink")

; Add SystemC include directories.
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/opt/systemc/include/")))))

;; ----------------------- Configure LiquidHaskell -------------

(add-to-list 'load-path "~/.emacs.d/liquid-tip.el/")

;; Configure flycheck-liquidhs, if you haven't already
(add-hook 'haskell-mode-hook
          '(lambda () (flycheck-select-checker 'haskell-liquid)))
          ;; '(lambda () (flycheck-select-checker 'haskell-stack-liquid)))
(add-hook 'literate-haskell-mode-hook
          '(lambda () (flycheck-select-checker 'haskell-liquid)))
          ;; '(lambda () (flycheck-select-checker 'haskell-stack-liquid)))

(require 'liquid-types)

;; Toggle minor mode on entering Haskell mode.
(add-hook 'haskell-mode-hook
          '(lambda () (liquid-types-mode)))
(add-hook 'literate-haskell-mode-hook
      '(lambda () (liquid-types-mode)))

;; -------------------------------------------------------------

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

;; Conal's tip on loading environment variables when launching from desktop.
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Saving a list of recently editted files.
;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; Misc.
(column-number-mode 1)
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

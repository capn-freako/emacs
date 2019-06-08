;; Emacs `~/.emacs.d/init.el` file, customized for use on AWS.

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(markdown-command "pandoc")
 '(package-selected-packages (quote (markdown-mode spice-mode)))
 '(save-place t)
 '(show-paren-mode t))

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
 '(font-lock-variable-name-face ((t (:foreground "MistyRose1"))))
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

;; initial window settings
(setq initial-frame-alist
      '((width . 100)
        (height . 54)
	(font . "Menlo-14")
        (background-color . "gray10")
        (foreground-color . "gray90")))

;; subsequent window settings
(setq default-frame-alist
      '((menu-bar-lines . 1)
        (tool-bar-lines . 0)
        (width . 100)
        (height . 52)
	(font . "Menlo-14")
        (background-color . "gray10")
        (foreground-color . "gray90")))

;; markdown-mode stuff
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

;; Misc.
(column-number-mode 1)
(savehist-mode 1)

;; spice-mode
(add-to-list 'load-path "~/.emacs.d/spice-mode/")
(require 'spice-mode)

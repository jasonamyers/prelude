;; Personal preferences
(setq bell-volume 0)
(prelude-require-package 'rainbow-delimiters)
(prelude-require-package 'smex)
(prelude-require-package 'company-go)
(prelude-require-package 'smart-mode-line)
(prelude-require-package 'elpy)
(prelude-require-package 'ein)
(prelude-require-package 'py-autopep8)
(prelude-require-package 'company-racer)
(prelude-require-package 'racer)
(prelude-require-package 'flycheck-rust)
(prelude-require-package 'rust-mode)
(prelude-require-package 'dracula-theme)

(load-theme 'dracula t)
(global-linum-mode t) ;; enable line numbers globally

;; Reduce the time after which the company auto completion popup opens
(setq company-idle-delay 0.2)

;; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 1)

(setq column-number-mode 't)
(setq sml/theme 'respectful)
(setq sml/shorten-modes 't)
(sml/setup)
(add-hook 'after-init-hook 'sml/setup)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun mkdir-if-not (dir)
  "Make directory if it doesn't exist"
  (unless (file-exists-p dir) (mkdir dir)))

;;; Configure backups
(mkdir-if-not "~/.emacs.d/backups")
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)


;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (setq whitespace-line-column 79)
              (setq whitespace-style '(face lines-tail))
              (whitespace-mode)
              (elpy-use-ipython))))

;;; Set default font
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Hack-15"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)
(add-to-list 'load-path "~/Misc/emacs/go-mode.el/")
(require 'go-mode-load)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/home/jasomyer/dev/go")
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/home/jasomyer/dev/go/bin")

(add-to-list 'load-path "/home/jasomyer/.emacs.d/lisp")
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (add-to-list 'ac-dictionary-directories "/home/jasomyer/.emacs.d/lisp/ac-dict")
;; (ac-config-default)

;; (require 'go-autocomplete)
(require 'company-go)

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

;; Set path to racer binary
(setq racer-cmd "/usr/local/bin/racer")
;; Set path to rust src directory
(setq racer-rust-src-path "/home/jasomyer/.rust/src/")
;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;; Setting up configurations when you load rust-mode

(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-tooltip-align-annotations t)

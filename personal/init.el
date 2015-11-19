;; Personal preferences
(setq bell-volume 0)
(prelude-require-package 'rainbow-delimiters)
(prelude-require-package 'smart-mode-line)
(prelude-require-package 'elpy)
(prelude-require-package 'dracula-theme)
(load-theme 'dracula t)

(elpy-enable)
(setq column-number-mode 't)
(setq sml/theme 'dark)
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

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (setq whitespace-line-column 79)
              (setq whitespace-style '(face lines-tail))
              (whitespace-mode))))

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

(setenv "GOPATH" "/Users/jasomyer/dev/go")
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/Users/jasomyer/dev/go/bin")

(add-to-list 'load-path "/Users/jasomyer/.emacs.d/lisp")
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/Users/jasomyer/.emacs.d/lisp/ac-dict")
(ac-config-default)

(require 'go-autocomplete)

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

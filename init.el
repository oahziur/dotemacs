;; It is better to do this at very begining to avoid UI changes
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq package-enable-at-startup nil)
(add-hook 'after-init-hook 'init-config)

(defun init-config ()
  ;; (setq debug-on-error t)
  (init-default)
  (init-os-x)
  (init-exec-path)
  (init-packages)
  (init-relative-line-numbers)
  (init-solarized-theme)
  (init-yasnippet)
  (init-auto-complete-mode)
  (init-helm-mode)
  (init-projectile-mode)
  (init-evil-mode)
  (init-general-editting)
  (init-javascript)
  (init-octave)
  (init-makefile)
  (init-daemon))

(defun init-exec-path ()
  "Set execute path for all programs"
  (add-to-list 'exec-path "/usr/local/bin/"))

(defun init-packages ()
  "Setup package archives and install packages"
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (setq url-http-attempt-keepalives nil)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (dolist (packages '(auto-complete
                      evil
                      evil-jumper
                      evil-visualstar
                      helm
                      htmlize
                      js-comint
                      projectile
                      relative-line-numbers
                      solarized-theme
                      yasnippet))
    (when (not (package-installed-p packages))
      (package-install packages))))

(defun init-yasnippet ()
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.snippet")
  (yas-global-mode 1)
  ;; Disable yas snippet mode in term mode, this will make terminal
  ;; works normal.
  (add-hook 'term-mode-hook
            (lambda()
              (setq yas-dont-activate t))))

(defun init-auto-complete-mode ()
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq
   ac-auto-start 2
   ac-override-local-map nil
   ac-use-menu-map t
   ac-set-trigger-key "TAB")
  (define-key ac-menu-map "\M-n" 'ac-next)
  (define-key ac-menu-map "\M-p" 'ac-previous)
  (setq
   ac-delay 0
   ac-auto-show-menu 0.1
   ac-quick-help-delay 0.1)
  (ac-flyspell-workaround))

(defun init-helm-mode ()
  (helm-mode 1)
  (global-set-key (kbd "C-c b") 'helm-buffers-list)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (require 'saveplace)
  (setq-default save-place t))

(defun init-projectile-mode ()
  (projectile-global-mode))

(defun init-evil-mode ()
  (require 'evil-visualstar)
  (require 'evil-jumper)
  (evil-mode 1)
  ;; Using 'jk' for switching from insert state to normal state
  (define-key evil-insert-state-map "j" #'cofi/maybe-exit)
  (evil-define-command cofi/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                             nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map "-u" 'undo-tree-visualize)
  (define-key evil-normal-state-map "z=" 'ispell-word)
  (define-key evil-normal-state-map "-b" 'helm-buffers-list)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
  (add-hook 'org-mode-hook
            (lambda()
              ;; insert list headings and checkbox for C-j
              (define-key evil-normal-state-map (kbd "C-j")
                'org-insert-heading)
              (define-key evil-insert-state-map (kbd "C-j")
                'org-insert-heading)
              (define-key evil-normal-state-map (kbd "C-S-j")
                'org-insert-todo-heading)
              (define-key evil-insert-state-map (kbd "C-S-j")
                'org-insert-todo-heading)))
  (setq evil-emacs-state-cursor  '("red" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("black" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-motion-state-cursor '("gray" box)))

(defun init-solarized-theme ()
  "Configuration for solarized theme"
  (set-face-attribute 'default nil :family "Source Code Pro" :height 110)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-use-less-bold t)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-dark t))

(defun init-default ()
  "Default behaviours"
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (custom-set-variables '(inhibit-startup-screen t)))

(defun init-os-x ()
  "Configuration for MAC OS X"
  (add-hook 'before-make-frame-hook
            (lambda ()
              (if (equal system-type 'darwin)
                  (progn
                    (add-to-list 'default-frame-alist '(left   . 0))
                    (add-to-list 'default-frame-alist '(top    . 0))
                    (add-to-list 'default-frame-alist '(height . 56))
                    (add-to-list 'default-frame-alist '(width  . 99)))))))

(defun init-relative-line-numbers ()
  (global-relative-line-numbers-mode)
  (setq relative-line-numbers-count-invisible-lines nil)
  (setq relative-line-numbers-format
        (lambda (offset)
          (if (= offset 0)
              " ~>"
            (format "%3d" (abs offset))))))

(defun init-general-editting ()
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)
  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        apropos-do-all t
        mouse-yank-at-point t
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
  (require 'flyspell)
  ;; flyspell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; whitespace cleanup on save
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; org-setting
  ;; source block highlighting
  (setq org-src-fontify-natively t)
  ;; display line and column number
  (setq line-number-mode t)
  (setq column-number-mode t)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro 4))

(defun init-javascript ()
  (setq js-indent-level 2)
  (require 'js-comint)
  (setq inferior-js-program-command
        "node -e require('repl').start({ignoreUndefined:true})")

  (setq inferior-js-mode-hook
        (lambda ()
          ;; colors
          (ansi-color-for-comint-mode-on)
          ;; Deal with some prompt nonsense
          (add-to-list
           'comint-preoutput-filter-functions
           (lambda (output)
             (replace-regexp-in-string "\033\\[[0-9]+[JGK]" "" output)))))
  (setenv "NODE_NO_READLINE" "1"))

(defun init-makefile ()
  (add-hook 'makefile-mode-hook (lambda ()
                                  (setq indent-tabs-mode 1))))

(defun init-octave ()
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(defun init-daemon ()
  (require 'server)
  (unless (server-running-p)
    (server-start)))

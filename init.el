(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq package-enable-at-startup nil)

(add-hook 'after-init-hook 'init-config)

(defun init-config ()
  ;; (setq debug-on-error t)
  (init-packages-config)
  (init-solarized-theme)
  (init-default)
  (init-frame-osx)
  (init-exec-path-osx)
  (init-relative-line-numbers)
  (init-yasnippet)
  (init-auto-complete)
  (init-helm)
  (init-projectile)
  (init-smex)
  (init-evil)
  (init-general-editting)
  (init-javascript)
  (init-clojure)
  (init-octave)
  (init-org)
  (init-swift)
  (init-coffee)
  (init-scala)
  (init-doc-view)
  (init-server))

(defun init-packages-config ()
  "Setup package archives."
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (setq url-http-attempt-keepalives nil)
  (when (not package-archive-contents)
    (package-refresh-contents)))

(defun init-package-require (package)
  "Install required package if not installed."
  (interactive)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

(defun init-exec-path-osx ()
  "Set execute path for OS X."
  (init-package-require 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun init-yasnippet ()
  (init-package-require 'yasnippet)
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.snippet")
  (yas-global-mode 1)
  ;; Disable yas snippet mode in term mode, this will make terminal
  ;; works normal.
  (add-hook 'term-mode-hook
            (lambda ()
              (setq yas-dont-activate t))))

(defun init-auto-complete ()
  (init-package-require 'auto-complete)
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
  (ac-flyspell-workaround)
  (global-auto-complete-mode)
  (add-to-list 'ac-modes 'octave-mode))

(defun init-helm ()
  (init-package-require 'helm)
  (helm-mode 1))

(defun init-projectile ()
  (init-package-require 'projectile)
  (projectile-global-mode))

(defun init-smex ()
  (init-package-require 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun init-evil ()
  (init-package-require 'evil)
  (init-package-require 'evil-visualstar)
  (init-package-require 'evil-jumper)
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
  (define-key evil-normal-state-map "-k" 'ido-kill-buffer)
  (define-key evil-normal-state-map "t" 'org-todo)
  (define-key evil-normal-state-map "-t" 'org-todo-list)
  (define-key evil-normal-state-map "-ff" 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
  (evil-ex-define-cmd "Q" 'evil-quit)
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "WQ" 'evil-save-and-close)
  (setq evil-emacs-state-cursor  '("red" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("black" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-motion-state-cursor '("gray" box))
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (when (buffer-file-name)
                (save-buffer)))))

(defun init-solarized-theme ()
  "Configuration for solarized theme"
  (when (window-system)
    (init-package-require 'solarized-theme)
    (set-face-attribute 'default nil :family "Source Code Pro" :height 110)
    (setq solarized-use-variable-pitch nil)
    (setq solarized-use-less-bold t)
    (setq solarized-height-minus-1 1.0)
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
    (load-theme 'solarized-dark t)))

(defun init-default ()
  "Default behaviours"
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (custom-set-variables '(inhibit-startup-screen t))
  (global-set-key (kbd "C-c b") 'helm-buffers-list)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (require 'saveplace)
  (setq-default save-place t)
  (setq-default fill-column 80))

(defun init-frame-osx ()
  "Configuration for MAC OS X."
  (when (memq window-system '(mac ns))
    (add-hook 'before-make-frame-hook
              (lambda ()
                (add-to-list 'default-frame-alist '(left   . 0))
                (add-to-list 'default-frame-alist '(top    . 0))
                (add-to-list 'default-frame-alist '(height . 56))
                (add-to-list 'default-frame-alist '(width  . 99))))))

(defun init-relative-line-numbers ()
  (init-package-require 'relative-line-numbers)
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
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (add-hook 'makefile-bsdmake-mode-hook 'whitespace-mode)
  (setq line-number-mode t)
  (setq column-number-mode t)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro 4))

(defun init-javascript ()
  (init-package-require 'js-comint)
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

(defun init-clojure ()
  (init-package-require 'clojure-mode)
  (init-package-require 'cider)
  (init-package-require 'ac-cider)
  (define-key evil-normal-state-map "-cj" 'cider-jack-in)
  (define-key evil-normal-state-map "-cc" 'cider-connect)
  (define-key evil-normal-state-map "-cq" 'cider-quit)
  (define-key evil-normal-state-map "-ck" 'cider-load-current-buffer)
  (define-key evil-normal-state-map "-ct" 'cider-test-run-tests)
  (define-key evil-normal-state-map "-cs" 'cider-repl-set-ns)
  (define-key evil-normal-state-map "-cd" 'cider-doc)
  (define-key evil-normal-state-map "-c." 'cider-jump-to-var)
  (define-key evil-normal-state-map "-c," 'cider-jump-back)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(defun init-octave ()
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(defun init-org ()
  (init-package-require 'htmlize)
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (setq org-src-fontify-natively t) ;; code block syntax highlight
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c s e") 'org-edit-src-code)
              (local-set-key (kbd "C-c C-t") 'org-insert-todo-heading)
              (local-set-key (kbd "C-j") 'org-insert-heading))))

(defun init-swift ()
  (init-package-require 'swift-mode))

(defun init-coffee ()
  (init-package-require 'coffee-mode))

(defun init-scala ()
  (init-package-require 'scala-mode2))

(defun init-doc-view ()
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (relative-line-numbers-mode -1)
              (setq doc-view-continuous t)
              (local-set-key (kbd "C-w h") 'windmove-left)
              (local-set-key (kbd "C-w l") 'windmove-right)
              (local-set-key (kbd "C-w j") 'windmove-down)
              (local-set-key (kbd "C-w k") 'windmove-up)
              (local-set-key (kbd "C-w s") 'split-window-vertically)
              (local-set-key (kbd "C-w v") 'split-window-horizontally)
              (local-set-key "j" 'doc-view-next-line-or-next-page)
              (local-set-key "k" 'doc-view-previous-line-or-previous-page)
              (local-set-key "h" 'image-backward-hscroll)
              (local-set-key "l" 'image-forward-hscroll)
              (local-set-key (kbd "C-d") 'image-scroll-up)
              (local-set-key (kbd "C-u") 'image-scroll-down)
              (local-set-key "/" 'doc-view-search)
              (local-set-key "?" 'doc-view-search-backward)
              (local-set-key "n" 'doc-view-search-next-match)
              (local-set-key "N" 'doc-view-search-previous-match))))

(defun init-server ()
  (when (window-system)
    (require 'server)
    (unless (server-running-p)
      (server-start)
      (when (memq window-system '(mac ns))
      ;; reserve at least one frame for keeping the server running for OS X only
        (suspend-frame)
        (new-frame)))))

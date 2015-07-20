(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq package-enable-at-startup nil)

(add-hook 'after-init-hook 'init-config)

(defun init-config ()
  ;; (setq debug-on-error t)
  (init-packages-config)
  (init-evil)
  ;; (init-solarized-theme)
  ;; (init-monokai-theme)
  (init-default)
  (init-exec-path-osx)
  (init-relative-line-numbers)
  (init-yasnippet)
  (init-auto-complete)
  (init-helm)
  (init-projectile)
  (init-smex)
  (init-general-editting)
  (init-javascript)
  (init-clojure)
  (init-octave)
  (init-org)
  (init-swift)
  (init-coffee)
  (init-scala)
  (init-racket)
  ;; (init-sml)
  (init-ocaml)
  (init-haskell)
  (init-markdown)
  (init-doc-view)
  ;; (init-eclim)
  ;; (init-jabber)
  (init-chinese-pyim)
  (init-tramp)
  (init-server))

(defun init-packages-config ()
  "Setup package archives."
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
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

(defun init-evil ()
  (init-package-require 'evil)
  (init-package-require 'evil-visualstar)
  (init-package-require 'evil-jumper)
  (require 'evil-visualstar)
  (require 'evil-jumper)
  (evil-mode 1)

  ;; Let me stay in insert mode longer.
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  (define-key evil-insert-state-map (kbd "M-f") 'forward-word)
  (define-key evil-insert-state-map (kbd "M-b") 'backward-word)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  
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

  (let ((new-evil-quit (lambda ()
                         (interactive)
                         (kill-this-buffer)
                         (evil-quit)))
        (new-evil-write-quit (lambda ()
                               (interactive)
                               (save-buffer)
                               (kill-this-buffer)
                               (evil-quit))))
    (evil-ex-define-cmd "q" new-evil-quit)
    (evil-ex-define-cmd "Q" new-evil-quit)
    (evil-ex-define-cmd "wq" new-evil-write-quit)
    (evil-ex-define-cmd "WQ" new-evil-write-quit))
 
  
  (evil-ex-define-cmd "W" 'evil-write)
 
  (setq evil-emacs-state-cursor  '("red" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("black" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-motion-state-cursor '("gray" box))

  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (let ((is-not-tramp-buffer
                     (lambda ()
                       (not (tramp-tramp-file-p
                             (buffer-file-name (current-buffer)))))))
                (when (and (buffer-file-name) (funcall is-not-tramp-buffer))
                  (save-buffer)))
              ))

  (init-package-require 'ace-jump-mode)
  (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-line-mode)
  )

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
  (init-package-require 'ac-ispell)
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq
   ac-auto-start 4
   ac-override-local-map nil
   ac-use-menu-map t)
  (define-key ac-menu-map "\M-n" 'ac-next)
  (define-key ac-menu-map "\M-p" 'ac-previous)
  (setq
   ac-delay 0
   ac-auto-show-menu 0.1
   ac-quick-help-delay 0.1)
  (ac-flyspell-workaround)

  ;; ac-ispell's config, only used for org mode
  (custom-set-variables
   '(ac-ispell-requires 4)
   '(ac-ispell-fuzzy-limit 4))

  (eval-after-load "auto-complete"
    '(progn
             (ac-ispell-setup))))

(defun init-helm ()
  (init-package-require 'helm)
  (define-key evil-normal-state-map "-b" 'helm-buffers-list)
  (helm-mode 1))

(defun init-projectile ()
  (init-package-require 'projectile)
  (define-key evil-normal-state-map "-ff" 'projectile-find-file)
  (projectile-global-mode))

(defun init-smex ()
  (init-package-require 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun init-solarized-theme ()
  "Configuration for solarized theme"
  (when (window-system)
    (init-package-require 'solarized-theme)
    (setq solarized-use-variable-pitch nil)
    (setq solarized-use-less-bold t)
    (setq solarized-height-minus-1 1.0)
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
    (load-theme 'solarized-dark t)))

(defun init-monokai-theme ()
  "Configureation for monokai-theme"
  (when (window-system)
    (init-package-require 'monokai-theme)
    (setq monokai-use-variable-pitch nil)
    (setq monokai-height-minus-1 1.0)
    (setq monokai-height-plus-1 1.0)
    (setq monokai-height-plus-2 1.0)
    (setq monokai-height-plus-3 1.0)
    (setq monokai-height-plus-4 1.0)
    (load-theme 'monokai t)))

(defun init-default ()
  "Default behaviours"
  (ido-mode t)
  (blink-cursor-mode 0)
  (setq ido-enable-flex-matching t)
  (custom-set-variables '(inhibit-startup-screen t))
  (global-set-key (kbd "C-c b") 'helm-buffers-list)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (require 'saveplace)
  (setq-default save-place t)
  (setq-default fill-column 80)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 130)
  (setq ns-pop-up-frames nil)
  (setq shell-file-name "/bin/sh"))

(defun init-relative-line-numbers ()
  (init-package-require 'relative-line-numbers)
  (define-key evil-normal-state-map "-r" 'relative-line-numbers-mode)
  (setq relative-line-numbers-motion-function #'forward-visible-line)
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
  (global-set-key (kbd "M-Q") 'unfill-paragraph)

  (define-key evil-normal-state-map "-u" 'undo-tree-visualize)
  (define-key evil-normal-state-map "z=" 'ispell-word)
  (define-key evil-normal-state-map "-k" 'ido-kill-buffer)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
  
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
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (add-hook 'makefile-bsdmake-mode-hook 'whitespace-mode)
  (setq line-number-mode t)
  (setq column-number-mode t)
  (global-auto-revert-mode t)

  (init-package-require 'smart-mode-line)
  (setq sml/theme 'light)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (set-face-background 'mode-line "dark grey")
  (set-face-background 'mode-line-inactive "light grey")
  
  (init-package-require 'google-c-style)
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

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
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (init-package-require 'clojure-mode)
  (init-package-require 'smartparens)
  (init-package-require 'cider)
  (init-package-require 'ac-cider)
  (init-package-require 'company)

  ;; Define evil mode local key maps
  (add-hook 'clojure-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map "-cj" 'cider-jack-in)
              (define-key evil-normal-state-local-map "-cc" 'cider-connect)
              (define-key evil-normal-state-local-map "-cq" 'cider-quit)
              (define-key evil-normal-state-local-map "-cb" 'cider-load-buffer)
              (define-key evil-normal-state-local-map "-ct" 'cider-test-run-tests)
              (define-key evil-normal-state-local-map "-cs" 'cider-repl-set-ns)
              (define-key evil-normal-state-local-map "-cd" 'cider-doc)
              (define-key evil-normal-state-local-map "-c." 'cider-jump-to-var)
              (define-key evil-normal-state-local-map "-c," 'cider-jump-back)
              ))
  
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (delete 'cider-mode ac-modes)
  (delete 'cider-repl-mode ac-modes)
  
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(defun init-octave ()
  (init-package-require 'ac-octave)
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(defun init-org ()
  (init-package-require 'htmlize)
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (setq org-src-fontify-natively t) ;; code block syntax highlight
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated nil)

  ;; add text highlight color for display markup text
  (add-to-list 'org-emphasis-alist
               '("/" (:background "CadetBlue2")))
  (add-to-list 'org-emphasis-alist
               '("_" (:background "light salmon")))
  
  ;; Disable company mode for now, intent to use when fuzzy complete is avaliable
  ;; (init-package-require 'company)
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (setq-local company-idle-delay 0)
  ;;                           (set (make-local-variable 'company-backends) '(company-ispell))))

  (add-hook 'org-mode-hook (lambda ()
                             (ac-ispell-ac-setup)
                             ;; Remove regular ac-ispell complete, only want to
                             ;; fuzzy one
                             (setq ac-sources (delete 'ac-source-ispell ac-sources))))
  
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map "gqq" 'fill-paragraph)
              (define-key evil-normal-state-local-map "t" 'org-todo)
              (define-key evil-normal-state-local-map "-t" 'org-todo-list)
              (define-key evil-normal-state-local-map (kbd "TAB") 'org-cycle)
              
              ;; Force refresh all images
              (define-key evil-normal-state-local-map "-d"  (lambda ()
                                                              (interactive)
                                                              (clear-image-cache)
                                                              (org-display-inline-images t t)))
              
              (define-key evil-normal-state-local-map "-i" 'org-toggle-inline-images)
              (local-set-key (kbd "C-c s e") 'org-edit-src-code)
              (local-set-key (kbd "C-c C-t") 'org-insert-todo-heading)
              (local-set-key (kbd "C-j") 'org-insert-heading)
              (local-set-key (kbd "C-c C-o") (lambda ()
                                               (interactive)
                                               (org-open-at-point)
                                               (evil-window-prev 1))))))

(defun init-swift ()
  (init-package-require 'swift-mode))

(defun init-coffee ()
  (init-package-require 'coffee-mode))

(defun init-scala ()
  (init-package-require 'scala-mode2))

(defun init-racket ()
  (init-package-require 'racket-mode))

(defun init-sml ()
  (init-package-require 'sml-mode))

(defun init-ocaml ()
  (init-package-require 'tuareg)
  (add-to-list 'auto-mode-alist '("\\.ml$" . tuareg-mode))) 

(defun init-haskell ()
  (init-package-require 'haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(defun init-markdown ()
  (init-package-require 'markdown-mode)
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

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

(defun init-eclim ()
  (init-package-require 'emacs-eclim)
  (require 'eclim)
  (require 'eclimd)
  (custom-set-variables
   `(eclimd-default-workspace "~/Documents/workspace")
   '(eclim-eclipse-dirs '("/Applications/eclipse"))
   '(eclim-executable "/Applications/eclipse/eclim"))
  ;; (global-eclim-mode)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  ;; add the emacs-eclim source
  (require 'ac-emacs-eclim-source)
  (global-set-key (kbd "C-SPC") 'eclim-complete))
  ;; (ac-emacs-eclim-config))

(defun init-jabber ()
  (init-package-require 'jabber)
  (load-file "~/.jabber-account-config.el")
  (custom-set-variables
   '(jabber-auto-reconnect t)
   '(jabber-avatar-verbose nil)
   '(jabber-vcard-avatars-retrieve nil)
   '(jabber-chat-buffer-format "*-jabber-%n-*")
   '(jabber-history-enabled t)
   '(jabber-mode-line-mode t)
   '(jabber-roster-buffer "*-jabber-*")
   '(jabber-roster-line-format " %c %-25n %u %-8s (%r)")
   '(jabber-show-offline-contacts nil)))

(defun init-chinese-pyim ()
  (init-package-require 'chinese-pyim)
  (require 'chinese-pyim)
  (setq default-input-method "chinese-pyim")
  ;; https://github.com/tumashu/chinese-pyim-bigdict/blob/master/pyim-bigdict.txt?raw=true
  (setq pyim-dicts
        '((:name "BigDict"
                 :file "~/.pyim-bigdict.txt"
                 :coding utf-8-unix)))
  (global-set-key (kbd "C-<SPC>") 'toggle-input-method))

(defun init-tramp ()
  (setq tramp-default-method "ssh"))

(defun init-server ()
  (when (window-system)
    (require 'server)
    (unless (server-running-p)
      (server-start))))

;; Some util functions
(defun ansi-term-fish ()
  (interactive)
  (ansi-term "fish"))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

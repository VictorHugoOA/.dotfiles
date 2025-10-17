(setq gc-cons-threshold (* 50 1024 1024))
(add-to-list 'default-frame-alist '(font . "BigBlueTerm437 Nerd Font Mono-15"))
(setq compilation-directory-locked nil)
(setq shell-command-swithc "-ic")
(setq dired-use-ls-dired nil)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(when (eq system-type 'darwin)
  (setq build-config-file "Config")
  (setq build-script "brazil-build release"))

(when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  (setq build-config-file "build.bat")
  (setq build-script "build.bat"))

(defun GetHomeDirectory()
  (if (eq system-type 'darwin) "/Users/volivett"
    (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
	"C:/Users/victo/AppData/Roaming/.emacs.d"
      "/home/hugovhoa")))

(defun LoadTodo()
  (interactive)
  (find-file-other-window (concat (GetHomeDirectory) "/todo.md"))
  (switch-to-buffer-other-window "todo.md"))

(defun NeverSplitAWindow()
  "Never split a window" nil)

(defun FindProjectDirectoryRecursive()
  (interactive)
  (if (file-exists-p build-config-file) t
    (cd "../")
    (FindProjectDirectoryRecursive)))

(defun CompilationHook()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil))

(defun LockCompilationDirectory()
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked"))

(defun UnlockCompilationDirectory()
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is unlocked"))

(defun FindProjectDirectory()
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (FindProjectDirectoryRecursive)
    (setq last-compilation-directory default-directory)))

(defun MakeWithoutAsking()
  (interactive)
  (if (FindProjectDirectory) (compile build-script))
  (other-window 1))

(defun OpenConfigFile()
  (interactive)
  (find-file-other-window (concat (GetHomeDirectory) "/../.emacs.el")))

(defun RevertAllBuffers()
  "Reload all buffer from disk quickly"
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename (not (buffer-modified-p buf)))
	(if (file-readable-p filename)
	    (with-current-buffer buf
	      (revert-buffer :ignore-auto :noconfirm :preserve-modes))
	  (let (kill-buffer-query-functions)
	    (kill-buffer buf)
	    (message "Killed non-existing/unreadable file buffer: %s" filename))))))
(message "Finished revertign buffers containig unmodified files."))

(defun OpenCurrentDirectory()
  (interactive)
  (dired "."))

(defun StartupTime()
  (message "Emacs loaded in %s with %d garbage collection."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(defadvice set-mark-command (after no-bloody-t-m-m activate)
  "Prevent consecutive marks activating the highlight"
  (if transient-mark-mode (setq transient-mark-mode nil)))

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq custom-file (concat (GetHomeDirectory) "/.emacs.custom.el"))
(setq project-bookmarks-file (concat (GetHomeDirectory) "/project-bookmarks.el"))
(setq project-list-file (concat (GetHomeDirectory) "/projects"))

(global-hl-line-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode)
(global-display-line-numbers-mode)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq electric-pair-mode t)
(setq electric-indent-mode nil)
(setq blink-cursor-mode nil)
(setq global-auto-revert-mode t)


(load-file custom-file)
(load-file project-bookmarks-file)

(require 'ido)
(require 'compile)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda() (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode lua-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown 0)
  (eglot-report-progress nil))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom 
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package corfu
  :custom 
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  (corfu-separator ?\s)
  (completion-ignore-case t)

  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion
)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init (vertico-mode))

(savehist-mode)

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))




(add-hook 'emacs-startup-hook #'StartupTime)
(add-hook 'compilation-mode-hook 'CompilationHook)


(ido-mode 1)
(split-window-horizontally)
(setq split-window-preferred-function 'NeverSplitAWindow)
(define-key global-map [f5] 'MakeWithoutAsking)
(define-key global-map (kbd "C-x d") 'OpenCurrentDirectory)
(define-key global-map (kbd "C-SPC") 'set-mark-command)
(define-key global-map (kbd "C-x C-f") 'ido-find-file-other-window)
(define-key global-map (kbd "C-x b") 'ido-switch-buffer-other-window)
(define-key global-map (kbd "C-x p p") '(lambda ()
					  (interactive)
					  (ListProjectsDirectories 'ListProjectsFromFile project-list-file)))
(define-key global-map (kbd "C-x p o") 'OpenProjectFromLine)
(define-key global-map (kbd "C-x p a") 'AddDirectoryToProjectList)

(LoadTodo)

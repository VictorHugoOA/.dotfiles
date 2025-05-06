(add-to-list 'default-frame-alist '(font . "BigBlueTerm437 Nerd Font Mono-15"))
(setq compilation-directory-locked nil)
(setq enable-local-varialbes nil)
(setq shell-command-swithc "-ic")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (eq system-type 'darwin)
  (setq build-config-file "Config")
  (setq build-script "brazil-build release")
  )

(when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  (setq build-config-file "build.bat")
  (setq build-script "build.bat")
  )

(defun GetHomeDirectory()
  (if (eq system-type 'darwin) "/Users/hugovhoa"
    (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
	"C:/Users/victo/AppData/Roaming/.emacs.d"
      "/home/hugovhoa"
      ))
)

(defun LoadTodo()
  (interactive)
  (find-file-other-window (concat (GetHomeDirectory) "/todo.md"))
  (switch-to-buffer-other-window "todo.md")
)

(defun NeverSplitAWindow()
  "Never split a window" nil)

(defun FindProjectDirectoryRecursive()
  (interactive)
  (if (file-exists-p build-config-file) t
    (cd "../")
    (FindProjectDirectoryRecursive)
    )
  )

(defun CompilationHook()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
  )

(defun LockCompilationDirectory()
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked")
  )

(defun UnlockCompilationDirectory()
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is unlocked")
  )

(defun FindProjectDirectory()
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (FindProjectDirectoryRecursive)
    (setq last-compilation-directory default-directory))
)

(defun MakeWithoutAsking()
  (interactive)
  (if (FindProjectDirectory) (compile build-script))
  (other-window 1)
  )

(defun OpenConfigFile()
  (interactive)
  (find-file-other-window (concat (GetHomeDirectory) "/../.emacs.el"))
  )

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

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq custom-file (concat (GetHomeDirectory) "/.emacs.custom.el"))
(setq java-lsp-file (concat (GetHomeDirectory) "/emacs-java.el"))

(global-hl-line-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode)
(global-display-line-numbers-mode)


(load-file custom-file)

(require 'ido)
(require 'compile)

(add-hook 'compilation-mode-hook 'CompilationHook)


(ido-mode 1)
(split-window-horizontally)
(setq split-window-preferred-function 'NeverSplitAWindow)
(define-key global-map [f5] 'MakeWithoutAsking)


(LoadTodo)
(load-file java-lsp-file)

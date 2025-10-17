(defun ListProjectsFromFile(projects-file)
  (interactive)
  (find-file-other-window projects-file))

(defun ListProjectsDirectories (list-directories-fun args)
  (interactive)
  (funcall list-directories-fun args))

(defun OpenProjectDirectory(directory)
  "Opens directory if the argument is a valid directory path"
  (if (file-directory-p directory)
      (dired directory)
    (message "Cannot find project directory")))

(defun AddDirectoryToProjectList ()
  (interactive)
  (let ((project-directory (concat default-directory "\n")))
    (append-to-file project-directory nil project-list-file)))

(defun OpenProjectFromLine()
  (interactive)
  (let (
	(project-directory (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	(project-file-name (buffer-file-name))
	)
    (if (not (string-equal project-file-name project-list-file))
	(message "Project does not come from project list")
      (OpenProjectDirectory project-directory))))


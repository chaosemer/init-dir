;;; init-dir.el ---  Init directory instead of just a single file -*- lexical-binding: t; -*-

;; Copyright 2005-2023 Jared Finder
;; Author:              Jared Finder <jared@finder.org>
;; Created:             Feb 22, 2005
;; Version:             0.2-beta
;; Keywords:            extensions, internal
;; URL:                 http://github.com/chaosemer/init-dir
;; Package-Requires:    ((emacs "27.1") (benchmark-init "1.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Keep all Emacs configuration in a separate directory that can be
;; under version control with very short init.el change.  Using
;; init-dir is as simple as putting the following single line in the
;; actual init.el file:
;;
;; (load-init-dir)
;;
;; This also comes with a very lightweight init profiler to warn you
;; if your Emacs startup is slow.

;;; Todo:
;;
;; At a high level, this package is intended to provide a smoother
;; init configuration experience.  There are a handful of improvements
;; that would be good to make to that end:
;;

;; Targeting 0.2:
;;
;; Capture runtime and garbage performance info, for display after
;; load.  Display would also have an easy way to reload that file and
;; get new perf data.
;;
;; Targeting 0.9:
;;
;; Add unit tests.

;;; Code:

;; Needed to byte compile cleanly.
(eval-when-compile (require 'benchmark-init-modes))

(defun init-dir--file-init-loadable-p (file)
  "Test if FILE should be loaded at Emacs initialization.

FILE: File path to test."
  (and (file-regular-p file)
       (member (file-name-extension file t) load-suffixes)))

(defun init-dir--directory-files-filter
    (directory predicate &optional full match nosort)
  "Return a list of files in DIRECTORY, excluding some files.
Files that don't match PREDICATE will not be included.

DIRECTORY: File path to a directory to list files in.
PREDICATE: Function to call on each file name.  Takes a single
  parameter, the filename, and returns if the file should be
  included in the results.
FULL: If non-nil, return absolute file names.  Otherwise,
  return names relative to the specified directory.
MATCH: A regexp or nil.  If non-nil, return only file names whose
  non-directory part matches this regexp.
NOSORT: If non-nil, the list is returned unsorted.  Otherwise,
  the list is returned sorted with `string-lessp'.

FULL, MATCH, NOSORT have the same meaning as in `directory-files'."
  (let ((files '()))
    (dolist (file (directory-files directory full match nosort))
      (when (funcall predicate file)
        (push file files)))
    (nreverse files)))

(defvar init-dir--long-load-time-warning 0.05
  "Controls if a file gets a warning if it takes too long to load.

Best practice is to increment this using `cl-incf' next to known
slow operations.  This can also be set to nil to completely
disable the long load warning.

Also see `init-dir-load'.")

(defvar init-dir--error-and-warning-list '()
  "Errors and warnings that came up while running `init-dir-load'.")

;;; The core functionality.
;;;###autoload
(defun init-dir-load (&optional dir)
  "Load files from DIR for initialization.

DIR: File path to a directory.  If unset or nil, DIR defaults
  to \"init\" in `user-emacs-directory'.  See info node `Find
  Init'.

The common use here is to have your init file be very short and
keep all configuration in a separate directory.  To use this
behavior, move your configuration to files inside one of these
directories and put just this single line in your init file:

\(init-dir-load)

Files will be loaded (via `load') in alphabetical order.  This is
intended to be used in your init file to load configuration that
is organized across multiple files.  A common pattern is to put
configuration for each mode in its own file.

This will display warnings whenever loading a single file from
the takes longer than `init-dir--long-load-time-warning'.  See
its documentation to see how to handle files known to take a long
time to load.

For your convenience this also runs `init-dir-check-packages' by
default.  If you do not want to run these checks, set
`init-dir-enable-package-checks' to nil."
  (setq dir (or dir (expand-file-name "init" user-emacs-directory))
        init-dir--error-and-warning-list '())
  (benchmark-init/activate)
  (unwind-protect
      (progn
        (dolist (file (delete-dups
                       (mapcar #'file-name-sans-extension
                               (init-dir--directory-files-filter
                                dir
                                #'init-dir--file-init-loadable-p
                                t))))
          (init-dir--load-single-file (init-dir--choose-as-load file) dir))

        ;; Package utilities.  This needs to be after loading files so
        ;; that it can be disabled via user init files.
        (when (and package-enable-at-startup
                   init-dir-enable-package-checks)
          (init-dir-check-packages)))
    (benchmark-init/deactivate))

  ;; Display any warnings.
  (when init-dir--error-and-warning-list
    (dolist (message (nreverse init-dir--error-and-warning-list))
      (display-warning 'init message))))

(defun init-dir--load-single-file (file root-dir)
  "Load a single file, with additional structure around it.

FILE: File path to a file to load.  Unlike `load', this must be
      an absolute path with an extension.
ROOT-DIR: Directory root being loaded from."
  (let (;; Dynamic binding intended to be modified by clients.
        (init-dir--long-load-time-warning init-dir--long-load-time-warning))

    (let* (;; This line actually loads the file as a side effect.
           (load-error
            (condition-case err
                (load file nil nil t t)
              (:success nil)
              ((debug t) err)))
           (node (init-dir--benchmark-init-node file))
           (duration (/ (benchmark-init/node-duration node) 1000.0)))
      (when load-error
        (push (format "Loading `%s' had an error: %S"
                      (init-dir--make-file-link file root-dir)
                      (error-message-string load-error))
	      init-dir--error-and-warning-list))
      (when (and init-dir--long-load-time-warning
                 (> duration init-dir--long-load-time-warning))
        (push (format "Loading `%s' took %f seconds. %s "
                      (init-dir--make-file-link file root-dir)
                      duration
                      (if (not (fboundp 'buttonize)) ;Requires GNU Emacs 29.1
                          ""
                        (buttonize "[Timing]" #'init-dir--show-timing file)))
              init-dir--error-and-warning-list)))))

(defun init-dir--make-file-link (file root-dir)
  "Return clickable text for a link to FILE.

The text will contain FILE, with the ROOT-DIR prefix removed.
Clicking the text will open the FILE, as if by `find-file'.

FILE: An absolute path to a file.
ROOT-DIR: Directory root that file is in."
  (if (not (fboundp 'buttonize))        ;Requires GNU Emacs 29.1
      file
    (buttonize (file-relative-name file root-dir)
               #'find-file
               file
               "Visit this file")))

(defun init-dir--choose-as-load (file)
  "Return FILE with the suffix `load' would add."
  (catch 'return
    (dolist (suffix load-suffixes)
      (let ((file-with-suffix (concat file suffix)))
        (when (file-exists-p file-with-suffix)
          (throw 'return file-with-suffix))))
    nil))

(defun init-dir--benchmark-init-node (file)
  "Return the node corresponding to FILE.
Return value is of type `benchmark-init/node'."
  (let ((abbrev-name (abbreviate-file-name file))
        (children (benchmark-init/node-children benchmark-init/durations-tree)))
    (seq-find (lambda (node) (string= abbrev-name
                                      (benchmark-init/node-name node)))
              children)))

(defun init-dir--show-timing (file)
  "Show the timing data for FILE.

This shows the tree for just the single node using
`benchmark-init/show-durations-tree' for debugging."
  ;; Only show the relevant node in the tree, for most analysis.
  ;; TODO: This should be an officially supported call.
  (require 'benchmark-init-modes)
  (let* ((node (init-dir--benchmark-init-node file))
         (benchmark-init/durations-tree node))
    ;; Force benchmark-init to refresh its buffer by destroying any
    ;; existing buffer.  This is a hack, but buffers being destroyed
    ;; by user is a common path, so this is unlikely to break.
    (when-let ((buf (get-buffer (format benchmark-init/buffer-name "Tree"))))
      (kill-buffer buf))

    (benchmark-init/show-durations-tree)))

;;;###autoload
(defun init-dir-check-packages ()
  "Check the state of packages for common issues.

Any issues detected are reported as warnings along with automatic
fix buttons (if supported).  This is normally called
automatically by `init-dir-load'."

  ;; Check for missing package installs.
  (let ((not-installed (seq-difference package-selected-packages
                                       package-activated-list)))
    (when not-installed
      (display-warning 'init
                       (format "%d missing package%s: %s %s "
                               (length not-installed)
                               (if (= (length not-installed) 1) "" "s")
                               (mapconcat #'symbol-name not-installed ", ")
                               (init-dir--make-install-packages-button)))))

  ;; Calculate the list of upgradable packages.  This takes a
  ;; noticeable amount of time, so defer until soon after
  ;; initialization is complete.
  (when (and (fboundp 'package-vc-p)    ;Requires GNU Emacs 29.1
             (fboundp 'package-upgrade) ;Requires GNU Emacs 29.1
             (fboundp 'package--upgradeable-packages)) ;Requires GNU Emacs 29.1
    (run-with-idle-timer
     1 nil
     (lambda ()
       (when-let ((list (seq-remove (lambda (elt)
                                      (seq-some #'package-vc-p
                                                (alist-get elt package-alist)))
                                    (package--upgradeable-packages))))
         (display-warning 'init
                          (format "%d upgradeable package%s: %s %s "
                                  (length list)
                                  (if (= (length list) 1) "" "s")
                                  (mapconcat #'symbol-name list ", ")
                                  (init-dir--make-upgrade-packages-button
                                   list))))))))

(defun init-dir--make-install-packages-button ()
  "Return clickable text to install missing packages."
  (if (not (fboundp 'buttonize))        ;Requires GNU Emacs 29.1
      ""
    (buttonize "[Fix]"
               (lambda (&rest _) (package-install-selected-packages))
               nil
               "Install all missing packages")))

(defun init-dir--make-upgrade-packages-button (packages)
  "Return clickable text to upgrade packages.

PACKAGES: List of package symbols to upgrade when the button is clicked."
  (if (and (not (fboundp 'buttonize))        ;Requires GNU Emacs 29.1
           (not (fboundp 'package-upgrade))) ;Requires GNU Emacs 29.1
      ""
    (buttonize "[Fix]"
               (lambda (list) (mapc 'package-upgrade ;FIXME: Using quote instead of function to
                                                     ;suppress byte-compiler warning on pre 29.1
                                    list))
               packages
               "Upgrade all packages")))

;;; Customize variables:

(defcustom init-dir-enable-package-checks t
  "Set to non-nil if `init-dir-load' should also perform package checks."
  :type 'boolean
  :tag "Enable package checks"
  :group 'initialization
  :link '(url-link https://github.com/chaosemer/init-dir)
  :package-version '(init-dir . "0.1"))

(provide 'init-dir)

;;; init-dir.el ends here

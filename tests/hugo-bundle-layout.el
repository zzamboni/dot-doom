;;; hugo-bundle-layout.el --- Regression tests for bundle assets -*- lexical-binding: t; -*-

;; Load after the Doom configuration, then run:
;;   M-x ert RET ^zz/hugo- RET
;; Tests write only to temporary directories, retained on failure.

(require 'ert)
(require 'ox-hugo)
(require 'org-attach)

(defmacro zz/hugo-test-site (&rest body)
  "Run BODY in a temporary Hugo site, preserving artifacts on failure."
  (declare (indent 0))
  `(let* ((root (make-temp-file "hugo-bundle-test-" t))
          (site (expand-file-name "site/" root))
          (source (expand-file-name "content-org/" site))
          (default-directory source)
          (org-id-locations-file (expand-file-name "org-id-locations" root))
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-files nil)
          (org-id-extra-files nil)
          (org-agenda-files nil)
          success)
     (make-directory source t)
     (make-directory (expand-file-name "static" site) t)
     (unwind-protect
         (prog1 (progn ,@body) (setq success t))
       (if success
           (delete-directory root t)
         (message "Failed Hugo test artifacts: %s" root)))))

(defun zz/hugo-test-write (path contents)
  "Create PATH with CONTENTS, including parent directories."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert contents)))

(ert-deftest zz/hugo-flat-assets-through-real-and-symlink-paths ()
  ;; This exercises the exporter, not only the advised copy helper.
  (zz/hugo-test-site
    (let ((alias (expand-file-name "alias" root)))
      (make-symbolic-link source alias)
      (zz/hugo-test-write (expand-file-name "images/nested/picture.png" source) "image bytes")
      (zz/hugo-test-write (expand-file-name "attachments/id/photo.png" source) "photo bytes")
      (zz/hugo-test-write (expand-file-name "attachments/id/document.pdf" source) "PDF bytes")
      (dolist (dir (list source (file-name-as-directory alias)))
        (with-temp-buffer
          (org-mode)
          (setq default-directory dir
                buffer-file-name (expand-file-name "post.org" dir))
          (insert (format "#+hugo_base_dir: %s\n#+hugo_section: post\n" site))
          (insert "* Post\n:PROPERTIES:\n:EXPORT_HUGO_BUNDLE: bundle\n:EXPORT_FILE_NAME: index\n:DIR: attachments/id\n:END:\n")
          (insert "[[file:images/nested/picture.png]]\n\n[[attachment:photo.png]]\n\n[[attachment:document.pdf][Download]]\n")
          (goto-char (point-max))
          (org-hugo-export-wim-to-md))
        (let ((bundle (expand-file-name "content/post/bundle/" site)))
          (should (equal (directory-files bundle nil "^[^.]")
                         '("document.pdf" "index.md" "photo.png" "picture.png")))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "index.md" bundle))
            (dolist (name '("picture.png" "photo.png" "document.pdf"))
              (goto-char (point-min))
              (should (search-forward name nil t)))
            (should-not (string-match-p "images/\\|attachments/" (buffer-string)))))))))

(ert-deftest zz/hugo-preserves-static-and-existing-bundle-resources ()
  (zz/hugo-test-site
    (with-temp-buffer
      (org-mode)
      (let* ((info (list :hugo-base-dir site :hugo-section "post" :hugo-bundle "bundle"))
             (shared (expand-file-name "static/shared/logo.png" site))
             (existing (expand-file-name "content/post/bundle/nested/existing.png" site)))
        (zz/hugo-test-write shared "shared asset")
        (zz/hugo-test-write existing "existing asset")
        (should (equal (org-hugo--attachment-rewrite-maybe shared info) "/shared/logo.png"))
        (should (equal (org-hugo--attachment-rewrite-maybe existing info) "/nested/existing.png"))
        (should-not (file-exists-p (expand-file-name "content/post/bundle/logo.png" site)))
        (should-not (file-exists-p (expand-file-name "content/post/bundle/existing.png" site)))))))

(ert-deftest zz/hugo-non-bundle-missing-and-unsupported-files ()
  (zz/hugo-test-site
    (with-temp-buffer
      (org-mode)
      (let* ((plain (list :hugo-base-dir site :hugo-section "post"))
             (bundle (append plain '(:hugo-bundle "bundle")))
             (image (expand-file-name "images/photo.png" source))
             (unsupported (expand-file-name "notes.xyzzy" source)))
        (zz/hugo-test-write image "image")
        (zz/hugo-test-write unsupported "text")
        (should (equal (org-hugo--attachment-rewrite-maybe image plain) "/ox-hugo/photo.png"))
        (should (equal (org-hugo--attachment-rewrite-maybe "missing.png" bundle) "missing.png"))
        (should (equal (org-hugo--attachment-rewrite-maybe unsupported bundle) unsupported))))))

(ert-deftest zz/hugo-refreshes-copied-assets ()
  (zz/hugo-test-site
    (with-temp-buffer
      (org-mode)
      (let* ((info (list :hugo-base-dir site :hugo-section "post" :hugo-bundle "bundle"))
             (asset (expand-file-name "images/photo.png" source))
             (destination (expand-file-name "content/post/bundle/photo.png" site)))
        (zz/hugo-test-write asset "first")
        (should (equal (org-hugo--attachment-rewrite-maybe asset info) "photo.png"))
        (zz/hugo-test-write asset "updated")
        (set-file-times destination (seconds-to-time 1))
        (should (equal (org-hugo--attachment-rewrite-maybe asset info) "photo.png"))
        (with-temp-buffer
          (insert-file-contents destination)
          (should (equal (buffer-string) "updated")))))))

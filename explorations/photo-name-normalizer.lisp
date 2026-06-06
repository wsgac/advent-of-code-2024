(in-package #:photo-name-normalizer)

(defun get-photos-from-dir (dir)
  (uiop:directory-files dir))

(defun filename-to-segments (filename)
  (str:split "[_|.]" filename :regex t))

(defun filename-transform-1 (filename)
  (let* ((segments (filename-to-segments filename))
         (selected (cons "IMG" (subseq segments 0 2))))
    (setf (a:lastcar selected)
          (concatenate 'string (a:lastcar selected) "000"))
    (format nil "~{~a~^_~}.jpg" selected)))

(defun filename-transform-2 (filename)
  (let* ((segments (filename-to-segments filename))
         (selected (subseq segments 0 3)))
    (format nil "~{~a~^_~}.jpg" selected)))

(defun copy-files-with-transform (source destination transform)
  (let ((destination (uiop:ensure-directory-pathname destination))
        (source-files (get-photos-from-dir source)))
    (uiop:ensure-all-directories-exist (list destination))
    (loop
      :for i from 1
      :for srcpath :in source-files
      :for dstpath := (funcall transform (pathname-name srcpath))
      :do (uiop:copy-file srcpath
                          (merge-pathnames dstpath destination))
      :do (format t "~%Copied (~a/~a) files" i (length source-files)))))

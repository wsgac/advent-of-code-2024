(in-package #:photo-name-normalizer)

(defun get-photos-from-dir (dir)
  (uiop:directory-files dir))

(defun filename-to-segments (filename)
  (str:split "[_|.]" filename :regex t))

(defun filename-transform-1 (filename)
  "Transform FILENAMEs of the form YYYYMMDD_hhmmss_<stuff>.jpg to
IMG_YYYYMMDD_hhmmdd000.jpg"
  (let* ((segments (filename-to-segments filename))
         (selected (cons "IMG" (subseq segments 0 2))))
    (setf (a:lastcar selected)
          (concatenate 'string (a:lastcar selected) "000"))
    (format nil "~{~a~^_~}.jpg" selected)))

(defun filename-transform-2 (filename)
  "Transform FILENAMEs of the form IMG_YYYYMMDD_hhmmss000_<stuff>.jpg to
IMG_YYYYMMDD_hhmmss000.jpg"
  (let* ((segments (filename-to-segments filename))
         (selected (subseq segments 0 3)))
    (format nil "~{~a~^_~}.jpg" selected)))

(defun unique-dstpath (destination dstpath)
  (let ((full-path (merge-pathnames dstpath destination)))
    (if (not (probe-file full-path))
        full-path
        (let* ((name (pathname-name dstpath))
               (base (subseq name 0 (- (length name) 3))))
          (loop
            :for suffix := (format nil "~3,'0d" (random 1000))
            :for candidate := (merge-pathnames
                               (concatenate 'string base suffix ".jpg")
                               destination)
            :until (not (probe-file candidate))
            :finally (return candidate))))))

(defun copy-files-with-transform (source destination transform)
  (let ((destination (uiop:ensure-directory-pathname destination))
        (source-files (get-photos-from-dir source)))
    (uiop:ensure-all-directories-exist (list destination))
    (loop
      :for i from 1
      :for srcpath :in source-files
      :for dstpath := (funcall transform (pathname-name srcpath))
      :for full-dstpath := (unique-dstpath destination dstpath)
      :do (uiop:copy-file srcpath full-dstpath)
      :do (format t "~%Copied (~a/~a) files" i (length source-files)))))

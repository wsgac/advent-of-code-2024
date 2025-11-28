(in-package #:m3u-downloader)

(defmethod extract-urls ((method (eql :file)) path-or-url)
  (let ((contents (a:read-file-into-string path-or-url)))
    (%extract-urls contents)))

(defmethod extract-urls ((method (eql :url)) path-or-url)
  (let ((contents (dex:get path-or-url :force-string t)))
    (%extract-urls contents)))

(defun %extract-urls (m3u-file-contents)
  (let* ((urls (remove-if-not (lambda (s) (str:starts-with? "http" s))
                              (uiop:split-string m3u-file-contents :separator '(#\newline)))))
    urls))

(defun extract-filename (segment-url)
  (subseq segment-url (1+ (position #\/ segment-url :from-end t))))

(defun download-segments (segments)
  (let* ((dirname (format nil "/tmp/~a/" (dexador.util:make-random-string)))
         (lparallel:*kernel* (lparallel:make-kernel 16)))
    (uiop:ensure-all-directories-exist (list dirname))
    (lparallel:pmapc (a:rcurry #'%download-segment dirname) segments)
    (uiop:parse-unix-namestring dirname)))

(defun %download-segment (segment-url dir)
  (let* ((stream (dex:get segment-url :want-stream t))
         (filename (extract-filename segment-url))
         (filepath (cl-fad:merge-pathnames-as-file dir filename)))
    (serapeum:write-stream-into-file stream filepath)))

(defun fetch-complete-audio (m3u-url output-name)
  (let* ((urls (extract-urls :url m3u-url))
         (segments-dir (download-segments urls))
         
         (file-list-file (cl-fad:merge-pathnames-as-file segments-dir "list.txt"))
         (output-filename-mp4 (cl-fad:merge-pathnames-as-file segments-dir
                                                              (make-pathname :name output-name
                                                                             :type "mp4")))
         (output-filename (cl-fad:merge-pathnames-as-file segments-dir
                                                          (make-pathname :name output-name
                                                                         :type "mp3"))))
    (a:with-output-to-file
                        (s file-list-file)
                      (loop
                        :for url :in urls
                        :for file := (extract-filename url)
                        :do (format s "file '~a'~%" file)))
    (uiop:run-program (format nil "ffmpeg -f concat -safe 0 -i ~a -c copy ~a"
                              file-list-file output-filename-mp4))
    (uiop:run-program (format nil "ffmpeg -i ~a -vn -acodec libmp3lame -q:a 2 ~a"
                              output-filename-mp4 output-filename))
    (uiop:rename-file-overwriting-target output-filename
                                         (cl-fad:merge-pathnames-as-file #p"/tmp/" (file-namestring output-filename)))
    (uiop:delete-directory-tree segments-dir :if-does-not-exist :ignore :validate t)))

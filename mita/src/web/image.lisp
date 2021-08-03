(defpackage :mita.web.image
  (:use :cl)
  (:export :find-image))
(in-package :mita.web.image)

(defun find-image-root (dep req image)
  (cadr (assoc (mita.image:image-source image)
               (list (list mita.image:+source-content+
                           (mita.web.dep:get-content-root dep req))
                     (list mita.image:+source-thumbnail+
                           (mita.web.dep:get-thumbnail-root dep req))))))

(defun find-image (dep req image-id-string
                   &key on-found
                        on-not-found)
  (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
    (if (not image-id)
        (funcall on-not-found)
        (let ((image (mita.db:with-connection (conn (mita.web.dep:get-db dep req))
                       (mita.image:load-image conn image-id))))
          (if (not image)
              (funcall on-not-found)
              (let ((root (find-image-root dep req image)))
                (if (not root)
                    (funcall on-not-found)
                    (let ((image-full-path
                           (parse-namestring
                            (format nil "~A/~A"
                                    root (mita.image:image-path image)))))
                      (funcall on-found image-full-path)))))))))

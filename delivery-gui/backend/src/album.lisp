(defpackage :mita.gui.album
  (:use :cl)
  (:export :update-state
           :loading
           :album
           :album-name
           :album-images
           :list-images
           :view
           :increment-index
           :set-index
           :viewing))
(in-package :mita.gui.album)

(defgeneric update-state (gateway state))

(defstruct (loading (:include mita.gui.state:loading)))

(defstruct album name images)

(defstruct (viewing (:include mita.gui.state:viewing))
  album)

(defmethod mita.gui.state:viewing-images ((v viewing))
  (album-images (viewing-album v)))

(defun list-images (album-id db gw)
  (update-state gw (make-loading))

  (mita.db:with-connection (conn db)
    (let ((album (mita.album:load-album-by-id conn album-id)))
      (update-state gw (make-album
                        :name (mita.album:album-name album)
                        :images (mita.album:album-images conn album))))))


(defun find-index (id images)
  (position id images
            :key #'mita.image:image-id
            :test #'mita.id:id=))

(defun view (album image-id gw)
  (update-state gw (make-viewing
                    :album album
                    :index (or (when image-id
                                 (find-index image-id (album-images album)))
                               0))))

(defun set-index (viewing image-id gw)
  (setf (viewing-index viewing)
        (or (find-index image-id
                        (album-images (viewing-album viewing)))
            0))
  (update-state gw viewing))

(defun increment-index (viewing diff gw)
  (let ((added-index (+ (viewing-index viewing) diff))
        (max-index (1- (length (mita.gui.state:viewing-images viewing)))))
    (let ((new-index (cond ((< added-index 0)
                            0)
                           ((< max-index added-index)
                            max-index)
                           (t
                            added-index))))
      (setf (viewing-index viewing) new-index)
      (update-state gw viewing))))

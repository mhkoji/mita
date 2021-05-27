(in-package :mita.db.bpt)

(defmethod mita.album:save-albums ((conn connection) (sources list))
  (let ((tree (album-tree conn)))
    (dolist (s sources)
      (let ((key (mita.id:to-string (mita.album:album-source-id s)))
            (value (list :id
                         (mita.album:album-source-id s)
                         :name
                         (mita.album:album-source-name s)
                         :created-on
                         (mita.album:album-source-created-on s)
                         :thumbnail
                         (mita.image:image-id
                          (mita.album:album-source-thumbnail s)))))
        (mita.util.b+tree:insert tree key value)))))

(defmethod mita.album:load-albums-in ((conn connection)
                                      (album-ids list))
  (when album-ids
    (setq album-ids (sort (mapcar #'mita.id:to-string album-ids)
                          #'string<))
    (let ((albums nil)
          (tree (album-tree conn))
          (first-key (car album-ids))
          (last-key (alexandria:lastcar album-ids)))
      (loop for node = (mita.util.b+tree:search-leaf-node tree first-key)
              then (aref (mita.util.b+tree::node-ptrs node)
                         (mita.util.b+tree::node-size node))
            while (and node
                       (not (mita.util.b+tree::key<
                             tree
                             last-key
                             (mita.util.b+tree::item-key
                              (aref (mita.util.b+tree::node-items node)
                                    0)))))
            do (let ((items (mita.util.b+tree::node-items node))
                     (size (mita.util.b+tree::node-size node)))
                 (loop for i from 0 below size
                       for item = (aref items i)
                       if (member (mita.util.b+tree::item-key item)
                                  album-ids
                                  :test (alexandria:curry
                                         #'mita.util.b+tree::key=
                                         tree))
                         do (let ((value (mita.util.b+tree::item-value
                                          item)))
                              (push (mita.album:construct-album
                                     :id (getf value :id)
                                     :name (getf value :name)
                                     :thumbnail
                                     #+nil ;; for debugging
                                     (getf value :thumbnail)
                                     (mita.image:load-image
                                      conn (getf value :thumbnail)))
                                    albums)))))
      albums)))

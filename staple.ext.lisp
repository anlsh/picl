(ql:quickload :staple-markdown)

(defclass my-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :picl))))
  'my-page)

(defmethod staple:packages ((system (eql (asdf:find-system :picl))))
  (mapcar #'find-package '(:picl)))

(defmethod staple:format-documentation ((docstring string) (page my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source docstring :markdown))))

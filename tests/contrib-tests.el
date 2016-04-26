;; ======================================================================
;; Unobtrusive completion insertion tests
;; ======================================================================

(ert-deftest unobtrusive-completion-insert ()
  (let ((eclim-insertion-functions '(eclim-unobtrusive-completion)))
    (cl-letf (((symbol-function 'eclim-java-import) #'ignore))
      (with-temp-buffer
        (insert "method(String arg1, List<String> arg2) - some.Class")
        (eclim--completion-action-java (line-beginning-position) (point))
        (should (equal (thing-at-point 'line) "method(_, _)"))
        (should (looking-at "_, _)"))
        (should (equal (mapcar #'(lambda(p) (get-text-property (point) p))
                               '(category help-echo face))
                       '(eclim-unobtrusive-placeholder
                         "method(String arg1, _)"
                         eclim-unobtrusive-placeholder-face)))
        (insert "typedAnArg")
        (should (equal (thing-at-point 'line) "method(typedAnArg, _)"))
        (forward-char 2)
        (should (looking-at "_)"))
        (should (equal (mapcar #'(lambda(p) (get-text-property (point) p))
                               '(category help-echo face))
                       '(eclim-unobtrusive-placeholder
                         "method(_, List<String> arg2)"
                         eclim-unobtrusive-placeholder-face)))
        (insert "thenSecond")
        (should (equal (thing-at-point 'line) "method(typedAnArg, thenSecond)"))
        ;; Properties should be gone
        (should (equal (next-single-property-change (point) 'category) nil))
        (should (equal (previous-single-property-change (point) 'category) nil))
        (end-of-buffer)
        ;; No args corner case.
        (insert "\nmethod2() - another.Class")
        (eclim--completion-action-java (line-beginning-position) (point))
        (should (equal (thing-at-point 'line) "method2()"))
        (should (eolp))
        ;; No properties.
        (should (equal (previous-single-property-change (point) 'category) nil))
        ))))

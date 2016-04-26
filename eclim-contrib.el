;; A grab-bag file of emacs-eclim enhancements from outside contributors that
;; haven't made it to the core feature set yet, or are marginal by nature.
;;
;; To report issues or post patches, use the repositories linked in the
;; "From:" header of every section, rather than the main Eclim repository.


;; ======================================================================
;; Unobtrusive completion insertion
;; From: github.com/ovy/emacs-eclim/
;;
;; (add-to-list 'eclim-insertion-functions 'eclim-unobtrusive-completion)
;; ======================================================================

(defface eclim-unobtrusive-placeholder-face
  '((t ( ;; :inherit font-lock-variable-name-face ;; More visible but garish
        :inherit shadow)))              ; Visible but subdued.
  "Face for eclim completion placeholders (`eclim-unobtrusive-completion')"
  :group 'eclim)
;; (makunbound 'eclim-unobtrusive-placeholder-face)

(defvar eclim-unobtrusive-placeholder-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f1") 'eclim-unobtrusive-javadoc)
    ;; Unlike company, we think C-h (+k) is useful right here. Use ? instead.
    (define-key map (kbd "?") 'eclim-unobtrusive-javadoc)
    map)
  "Keymap while cursor is on `eclim-unobtrusive-completion' placeholders.")
;; (makunbound 'eclim-unobtrusive-placeholder-keymap)

;; The category/plist mechanism is weird but seems like the right thing to do.
(unless (symbol-plist 'eclim-unobtrusive-placeholder) ;; like a defvar
  (setplist 'eclim-unobtrusive-placeholder
            '(face eclim-unobtrusive-placeholder-face
              font-lock-face eclim-unobtrusive-placeholder-face
              ;; display "…" ;; misbehaves when cut, a little too much magic
              insert-in-front-hooks (eclim-unobtrusive-placeholder-vanish)
              evaporate t rear-nonsticky t field t)))

(defun eclim-unobtrusive-completion (template)
  "An insertion function (alternative to yas) for
`eclim-complete' that tries to minimize visual disruption, while
being as useful as possible. Replaces the arguments with
underscores that are easy to understand and syntactically
correct (helps Eclipse), but have emacs text properties making
them politely disappear when you type. Each underscore has local
help text documenting the argument; it helps if
`help-at-pt-timer' is enabled, like for problem text. Takes a
yas-style TEMPLATE argument although it only supports named
arguments.

Keybindings while cursor is on the placeholder:
f1, ?: pull up javadoc for the original completion
\\[display-local-help] (display-local-help): (re)display short \
help for this argument

To enable, add it to `eclim-insertion-functions'."
  (let ((beg (point-marker)) (end (make-marker))
        replacements empty-template)
    (insert template)
    (set-marker end (point))
    (goto-char beg)
    (save-match-data
      (while (re-search-forward "\\${\\([^}]*\\)}" end t)
        (if (string= (match-string 1) "") ; template corner case
            (replace-match "" t)
          (push (cons (match-beginning 0) (match-string 1)) replacements)
          (replace-match "_" t))))
    ;; NB: placeholders is reversed. Suits us fine.
    (setq empty-template (buffer-substring-no-properties beg end))
    (dolist (pa replacements)
      (let* ((empty-pos (- (car pa) beg))
             (arg-help (concat (substring empty-template 0 empty-pos)
                               (propertize (cdr pa) 'face 'italic)
                               (substring empty-template (+ 1 empty-pos)))))
        (add-text-properties (car pa) (+ 1 (car pa))
          `(category eclim-unobtrusive-placeholder help-echo ,arg-help
            keymap ,eclim-unobtrusive-placeholder-keymap
            ;; Keeps markers alive until _ is deleted. No big deal in the 2010s.
            eclim-completion-began ,beg
            eclim-completion-ended ,end))
        ;; Unfortunately, due to a bug in font-lock, insert-in-front-hooks
        ;; are clobbered when font lock is active. For now, use an overlay.
        (let ((o (make-overlay (car pa) (+ 1 (car pa)) nil t)))
          (overlay-put o 'insert-in-front-hooks
                           '(eclim-unobtrusive-placeholder-vanish))
          (overlay-put o 'evaporate t)  ; dies when placeholder dies.
          ;; Make help text stronger than Eclipse problems if they show up,
          ;; since "_ can't be resolved to a variable" is not as helpful.
          (overlay-put o 'kbd-help arg-help)
          (overlay-put o 'priority 1000)
        )))
    (goto-char (or (car (car (last replacements))) end))
    (when replacements (display-local-help)) ;; otherwise msg is "Wrote file..."
    t))

(defun eclim-unobtrusive-placeholder-vanish (&rest ignored)
  "Deletes the argument placeholder that cursor is on (left by
`eclim-unobtrusive-completion'). Does nothing anywhere else."
  (interactive)
  (when (eq (get-text-property (point) 'category)
            'eclim-unobtrusive-placeholder)
    ;; Allow for longer placeholders in the future.
    (delete-region (point)
                   (next-single-property-change (point) 'category nil))))


(defun eclim-unobtrusive-javadoc ()
  "When called on an `eclim-unobtrusive-completion' placeholder,
pulls up javadoc for the completion that created it. This will
typically succeed, but may fail if the current state of the code
is too confusing for Eclipse. Also shows the help for the current
argument in the echo area. Note: performs a save, like the completion
did originally, to sync to Eclipse."
  (interactive)
  (let ((where-to-doc (get-text-property (point) 'eclim-completion-began))
        ;; We *know* the code is not correct here: there's an _ in place
        (eclim-autoupdate-problems nil) (found-javadoc nil))
    (when where-to-doc
      (setq found-javadoc
            (save-excursion
              (with-selected-window (selected-window)
                (goto-char where-to-doc)
                (eclim-java-show-documentation-for-current-element)))))
      ;; Two things: replace javadoc key msg with something helpful here;
      ;; document that we tried, but the state was probably too confused.
      (message "%s%s" (help-at-pt-string)
               (if found-javadoc "" " [no javadoc in this state]"))))

(provide 'eclim-contrib)

((nil
  . ((defun clerk-show ()
       (interactive)
       (when-let
           ((filename
             (buffer-file-name)))
         (save-buffer)
         (cider-interactive-eval
          (concat "(nextjournal.clerk/show! \"" filename "\")"))))

     (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))))

;; A minimal mode for interaction between Ciao and elisp.
;; Jose F. Morales

;; TODO: This is just a prototype.
;; TODO: Messages can be lost.
;; TODO: Add more flexible ways to start the buffers (e.g.,
;;       calling 'emacsclient' from the Ciao program) 

;; Some local variables:
;;
;;    ciao-elisp-proc:
;;      The underlying Ciao process associated to this buffer
;;      (note that the variable is buffer-local)
;;
;;    ciao-elisp-procbuff:
;;      The buffer name for the Ciao process

;; Interaction with the Ciao process
(defun ciao-elisp-filter (process output)
  (if (equal output "SET_STATE_EVENT_WAIT\n")
      t ;; Do nothing, events will sent the string to 'process'
    ;; Else, evaluate the output as an elisp code
    (ciao-elisp-event (eval (read output)))))

(defun ciao-elisp-event (ev)
  (process-send-string ciao-elisp-proc (concat (prin1-to-string ev) "\n")))

(defun ciao-elisp-run ()
  "Run a Ciao process that uses the elisp interface"
  (interactive)
  (let ((cmd (read-file-name "Command to execute: "))
	(my-buff (generate-new-buffer "*ciao-elisp*")))
    (switch-to-buffer my-buff)
    (make-local-variable 'ciao-elisp-procbuff)
    (make-local-variable 'ciao-elisp-proc)
    (setq ciao-elisp-procbuff (concat "*proc*" (buffer-name)))
    (setq ciao-elisp-proc
	  (start-process
	   ciao-elisp-procbuff ; process name
	   ciao-elisp-procbuff ; buffer name
	   cmd))
    (set-process-filter ciao-elisp-proc 'ciao-elisp-filter)
    ;
    (add-hook 'kill-buffer-hook 'exit-function nil t)
    ))

(defun ciao-elisp-stop ()
  "Stop the current Ciao process associated to this buffer"
  (interactive)
  (delete-process ciao-elisp-proc)
  (kill-buffer ciao-elisp-procbuff)
  )

(defun exit-function ()
  (ciao-elisp-stop))

(provide 'ciao-elisp)

(concat (buffer-name) "*proc*")
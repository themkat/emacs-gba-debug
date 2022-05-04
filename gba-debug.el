;;; gba-debug.el --- Better GBA debugging in Emacs
;; Version: 0.0.1

;; Simple utilities to make GBA debugging more pleasant
;; Uses compile and dap-gdb-lldb for ease of use

(require 'dap-mode)
(require 'dap-gdb-lldb)
(require 'f)

(defcustom gba-debug-gdb-path "/opt/devkitpro/devkitARM/bin/arm-none-eabi-gdb"
  "Path to the DevkitARM GDB executable (including executable)"
  :group 'gba-debug
  :type 'string)

(defcustom gba-debug-mgba-path "/Applications/mGBA.app/Contents/MacOS/mGBA"
  "Path to the mGBA executable (including executable)"
  :group 'gba-debug
  :type 'string)

;; base-template that will be used to launch a debug session with the right parameters
(dap-register-debug-template "GBA debug"
                             (list :name "GBA debug"
                                   :type "gdbserver"
                                   :request "attach"
                                   :gdbpath gba-debug-gdb-path
                                   :target ":2345"))

(defun gba-debug--run-debugger ()
  ;; TODO: rewrite so car won't fail on empty list
  (let* ((project-directory (locate-dominating-file default-directory "Makefile"))
         (elf-file (car (f-glob "*.elf" project-directory))))
    (dap-debug-edit-template (list :name "GBA debug"
                                   :executable elf-file
                                   :cwd project-directory))))

(defun gba-debug--run-mgba ()
  (let* ((project-directory (locate-dominating-file default-directory "Makefile"))
         (gba-file (car (f-glob "*.gba" project-directory))))
    (shell-command (string-join (list gba-debug-mgba-path " --gdb " gba-file)))))

(defun gba-debug--handle-compilation-make-buffer (buffer msg)
  (if (string-match "^finished" msg)
      (progn (delete-windows-on buffer)
             (delete 'gba-debug--handle-compilation-make-buffer compilation-finish-functions)

             ;; compilation succesful, start mgba
             ;; TODO: are we back to our previous buffer always at this point?
             (gba-debug--run-mgba)
             ;; TODO: are delays needed? better method to make sure mGBA has started?
             (sleep-for 5)
             (gba-debug--run-debugger))))

(defun gba-debug-program ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "Makefile")))
    (add-to-list 'compilation-finish-functions 'gba-debug--handle-compilation-make-buffer)
    (compile "make")))

(provide 'gba-debug)
;;; gba-debug.el ends here

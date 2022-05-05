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

(defun gba-debug--get-file-of-type (type directory)
  (let ((filematches (f-glob (string-join (list "*." type)) project-directory)))
    (if (zerop (length filematches))
        (error (string-join (list "Could not find " type " file! Was compilation succesful?")))
      (car filematches))))

(defun gba-debug--run-debugger ()
  (let* ((project-directory (f-full (locate-dominating-file default-directory "Makefile")))
         (elf-file (f-filename (gba-debug--get-file-of-type "elf" project-directory))))
    (dap-debug (list :name "GBA debug"
                     :type "gdbserver"
                     :request "attach"
                     :gdbpath gba-debug-gdb-path
                     :target ":2345"
                     :executable elf-file
                     :cwd project-directory))))

(defun gba-debug--run-mgba ()
  (let* ((project-directory (locate-dominating-file default-directory "Makefile"))
         (gba-file (gba-debug--get-file-of-type "gba" project-directory))
         (display-buffer-alist (list (list "\\*Async Shell Command\\*.*" #'display-buffer-no-window))))
    (async-shell-command (string-join (list gba-debug-mgba-path " --gdb " gba-file)))))

(defun gba-debug--handle-compilation-make-buffer (buffer msg)
  (if (string-match "^finished" msg)
      (progn 
        ;; compilation succesful, start mgba
        (gba-debug--run-mgba)
        (gba-debug--run-debugger)
        
        (kill-buffer buffer)
        (delete 'gba-debug--handle-compilation-make-buffer compilation-finish-functions))))

(defun gba-debug-program ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "Makefile"))
        (display-buffer-alist (list (list "\\*compilation\\*" #'display-buffer-no-window))))
    (add-to-list 'compilation-finish-functions 'gba-debug--handle-compilation-make-buffer)
    (compile "make")))

(provide 'gba-debug)
;;; gba-debug.el ends here

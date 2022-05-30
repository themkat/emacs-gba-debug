;;; gba-debug.el --- Better GBA debugging

;; URL: https://github.com/themkat/emacs-gba-debug
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dap-mode "0.7") (f "0.20.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple utilities to make GBA debugging more pleasant.
;; Uses compile and dap-gdb-lldb for ease of use

;;; Code:

(require 'dap-mode)
(require 'dap-gdb-lldb)
(require 'f)

(defcustom gba-debug-gdb-path "/opt/devkitpro/devkitARM/bin/arm-none-eabi-gdb"
  "Path to the DevkitARM GDB executable (including executable)."
  :group 'gba-debug
  :type 'string)

(defcustom gba-debug-mgba-path "/Applications/mGBA.app/Contents/MacOS/mGBA"
  "Path to the mGBA executable (including executable)."
  :group 'gba-debug
  :type 'string)

(defcustom gba-debug-build-command "make"
  "Build command to use. make by default."
  :group 'gba-debug
  :type 'string)

(defcustom gba-debug-projectfile "Makefile"
  "Project build file. Examples: Makefile, Cargo.toml etc."
  :group 'gba-debug
  :type 'string)

;; TODO: is some sort of custom executable path the best way to solve this?
;;       Does not feel like intricate logic to find the executable is worth it...
(defcustom gba-debug-custom-executable-path nil
  "Path (relative) to the executable we want to run. Assumes elf-type. Unused if nil."
  :group 'gba-debug
  :type 'string)

(defun gba-debug--get-file-of-type (type directory)
  "Gets a file in directory `DIRECTORY' with the extension `TYPE' if it exists."
  (let ((filematches (f-glob (string-join (list "*." type)) directory)))
    (if (zerop (length filematches))
        (error (string-join (list "Could not find " type " file! Was compilation succesful?")))
      (car filematches))))

(defun gba-debug--run-debugger ()
  "Runs the debugger."
  (let* ((project-directory (f-full (locate-dominating-file default-directory gba-debug-projectfile)))
         (elf-file (or gba-debug-custom-executable-path
                       (f-filename (gba-debug--get-file-of-type "elf" project-directory)))))
    (dap-debug (list :name "GBA debug"
                     :type "gdbserver"
                     :request "attach"
                     :gdbpath gba-debug-gdb-path
                     :target ":2345"
                     :executable elf-file
                     :cwd project-directory))))

(defun gba-debug--run-mgba ()
  "Starts the emulator in a background shell command buffer."
  (let* ((project-directory (locate-dominating-file default-directory gba-debug-projectfile))
         (gba-file (or gba-debug-custom-executable-path
                       (gba-debug--get-file-of-type "gba" project-directory)))
         (display-buffer-alist (list (list "\\*Async Shell Command\\*.*" #'display-buffer-no-window))))
    (async-shell-command (string-join (list gba-debug-mgba-path " --gdb " gba-file)))))

(defun gba-debug--handle-compilation-make-buffer (buffer msg)
  "Handles the result of the compilation; if succesful we should start a debug session."
  (if (string-match "^finished" msg)
      (progn
        ;; compilation succesful, start mgba
        (gba-debug--run-mgba)
        (gba-debug--run-debugger)
        
        (kill-buffer buffer)
        (delete 'gba-debug--handle-compilation-make-buffer compilation-finish-functions))))

(defun gba-debug-program ()
  "Start a GBA debug session. Will compile, start emulator, and open debug session."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory gba-debug-projectfile))
        (display-buffer-alist (list (list "\\*compilation\\*" #'display-buffer-no-window))))
    (add-to-list 'compilation-finish-functions 'gba-debug--handle-compilation-make-buffer)
    (compile gba-debug-build-command)))

(provide 'gba-debug)
;;; gba-debug.el ends here

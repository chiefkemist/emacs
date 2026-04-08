;;; core-dap.el --- Debug adapter protocol setup -*- lexical-binding: t; -*-

(require 'subr-x)

(defun chief/lldb-dap-command ()
  "Return the preferred LLDB DAP command list when available."
  (or (when-let* ((lldb-dap (executable-find "lldb-dap")))
        (list lldb-dap))
      (when-let* ((lldb-vscode (executable-find "lldb-vscode")))
        (list lldb-vscode))
      (when (and (eq system-type 'darwin)
                 (executable-find "xcrun"))
        (ignore-errors
          (when-let* ((output (car (process-lines "xcrun" "-f" "lldb-dap")))
                      (path (string-trim output))
                      ((file-executable-p path)))
            (list path))))))

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip repl))
  (dap-auto-configure-mode 1)
  (require 'dap-dlv-go)
  (require 'dap-netcore)
  (require 'dap-node)
  (require 'dap-python)
  (when-let* ((lldb-command (chief/lldb-dap-command)))
    (setq dap-lldb-debug-program lldb-command)
    (require 'dap-lldb))
  (when (executable-find "gdb")
    (require 'dap-gdb))
  (setq dap-python-debugger 'debugpy)
  (global-set-key (kbd "<f5>") #'dap-debug)
  (global-set-key (kbd "<f6>") #'dap-continue)
  (global-set-key (kbd "<f7>") #'dap-next)
  (global-set-key (kbd "<f8>") #'dap-step-in)
  (global-set-key (kbd "<f9>") #'dap-step-out)
  (chief/leader-def
    "d" '(:ignore t :which-key "debug")
    "dd" #'dap-debug
    "db" #'dap-breakpoint-toggle
    "dB" #'dap-breakpoint-condition
    "dc" #'dap-continue
    "di" #'dap-step-in
    "do" #'dap-step-out
    "dn" #'dap-next
    "dr" #'dap-debug-restart
    "dq" #'dap-disconnect
    "du" #'dap-ui-repl))

(use-package dap-ui
  :straight nil
  :after dap-mode
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

(provide 'core-dap)
;;; core-dap.el ends here

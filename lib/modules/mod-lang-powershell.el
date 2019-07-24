;;; mod-lang-powershell.el --- Configuration for PowerShell  -*- lexical-binding: t; -*-

(use-package powershell
  :ensure t
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

(provide 'mod-lang-powershell)

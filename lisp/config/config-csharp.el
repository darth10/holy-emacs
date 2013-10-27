;;; Configuration for C#

(require 'csharp-mode)
(require 'config-common)

(defconfig configure-csharp
  (auto-complete-mode)
  (c-set-style "c#"))

(add-hook 'csharp-mode-hook 'configure-csharp)

(provide 'config-csharp)

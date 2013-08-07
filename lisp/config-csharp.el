;;; Configuration for C#

(require 'csharp-mode)
(require 'config-common)

(defconfig configure-csharp)
(add-hook 'csharp-mode-hook 'configure-csharp)

(provide 'config-csharp)

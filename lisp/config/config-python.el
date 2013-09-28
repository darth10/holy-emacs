;;; Configuration for Python

(require 'config-common)

(defconfig configure-py)
(add-hook 'python-mode-hook 'configure-py)

(provide 'config-python)

;;; Configuration for Java

(defconfig configure-java
  (c-set-style "k&r")
  ;; (smartparens-mode)
  )

(add-hook 'java-mode-hook 'configure-java)

(provide 'config-java)

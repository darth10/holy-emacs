;;; Configuration for Java

(defconfig configure-java
  (c-set-style "k&r"))

(add-hook 'java-mode-hook 'configure-java)

(provide 'config-java)

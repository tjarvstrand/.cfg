(setq comint-buffer-maximum-size 15000)

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-output-filter-functions #'ansi-color-process-output)

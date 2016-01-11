;;load ramsa.lsp from network

(setq 
  rsa#path "\\\\ramsacfs\\project\\reference\\autocad\\2014\\" ; <= MODIFY FOR VERSION HERE
  rsa#support (strcat rsa#path "Support\\")
)

(load (strcat rsa#support "ramsa.lsp"))


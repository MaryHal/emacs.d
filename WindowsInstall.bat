rmdir /S %USERPROFILE%\AppData\Roaming\.emacs.d\
mkdir %USERPROFILE%\AppData\Roaming\.emacs.d\
xcopy /E /Y * %USERPROFILE%\AppData\Roaming\.emacs.d\
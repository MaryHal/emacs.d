choco install emacs64 -y

mkdir "%USERPROFILE%\AppData\Roaming\.emacs.d\"
mkdir "%USERPROFILE%\AppData\Roaming\.emacs.d\cache\"
mkdir "%USERPROFILE%\AppData\Roaming\.emacs.d\server\"
xcopy /E /Y * "%USERPROFILE%\AppData\Roaming\.emacs.d\"
mklink /D "%USERPROFILE%\.emacs.d" "%USERPROFILE%\AppData\Roaming\.emacs.d"

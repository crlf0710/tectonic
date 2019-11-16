rem Copyright 2019 the Tectonic Project
rem Licensed under the MIT License.
rem
rem This script installs vcpkg on windows os at %VCPKG_ROOT% directory

if not exist %VCPKG_ROOT% mkdir %VCPKG_ROOT%
cd %VCPKG_ROOT%
git init
git remote add origin https://github.com/crlf0710/tectonic-prebuilt-deps.git
git fetch origin master
git checkout -f -B master origin/master
FOR /D %%G in ("packages\*") DO xcopy /e /f /y "%%~G\*" "installed\%VCPKG_DEFAULT_TRIPLET%\"
if not exist installed\vcpkg\updates mkdir installed\vcpkg\updates

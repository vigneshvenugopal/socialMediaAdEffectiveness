@echo OFF
pushd %~dp0
python fetchSocailMediaData.py %*
pause
popd

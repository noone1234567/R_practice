#!/bin/bash

# Указываем путь к исполняемому файлу R
# Обычно он находится здесь, но путь может отличаться в вашей системе
R_EXECUTABLE="/usr/bin/Rscript"

R_SCRIPT_PATH="myscript.R"
$R_EXECUTABLE "$R_SCRIPT_PATH"

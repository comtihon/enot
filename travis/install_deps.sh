#!/usr/bin/env bash
if [ ! -d  "$HOME/artifactory/artifactory" ]; then
  mkdir -p $HOME/artifactory
  artifactorywget https://bintray.com/jfrog/artifactory/download_file?file_path=jfrog-artifactory-oss-5.3.0.zip \
       -O $HOME/artifactory/artifactory.zip
  unzip $HOME/artifactory/artifactory.zip -d $HOME/artifactory
  mv $HOME/artifactory/artifactory-oss-* $HOME/artifactory/artifactory
  echo 'true' > $HOME/artifactory/just_installed
else
  rm -f $HOME/artifactory/just_installed
  echo "Using cached artifactory."
fi
#!/bin/bash

function printYesNoPrompt() {
  echo "$1"
  echo "Enter 1 for Yes, 2 for No"
  select yn in "Yes" "No"; do
      case $yn in
          Yes ) break;;
          No ) exit;;
      esac
  done
}

function commitTagPush(){

  echo "Changing directory to $1"
  cd "$1"

  git status
  git branch

  printf "\n\n%s\n\n" "$1"
  printYesNoPrompt "Proceed with adding all, commit and push?"

  if [ -z "$COMMIT_MESSAGE" ]; then
    echo "No commit message was set - skipping commit";
  else
    git add .
    git commit -m "$COMMIT_MESSAGE"
    git push
  fi

  if [ -z "$TAG" ]; then
    echo "No tag was set - skipping tag";
  else
    git tag "$TAG"
    git push --tags
  fi
}

echo "Enter commit message  - If you enter nothing, no commit will be made."

read COMMIT_MESSAGE

echo "Enter tag e.g 0.0.8  - If you enter nothing, no tag will be applied."

read TAG

commitTagPush "$HOME/Documents/dev/java/rate-limiter"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-annotation"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-web-core"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-javaee"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-spring"

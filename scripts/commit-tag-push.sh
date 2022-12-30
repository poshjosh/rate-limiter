#!/bin/bash

function printYesNoPrompt() {
  printf "\n%s\n" "$1"
  printf "\nEnter 1 for Yes, 2 for No\n"
  select yn in "Yes" "No"; do
      case $yn in
          Yes ) break;;
          No ) exit;;
      esac
  done
}

function getGitCurrentBranchName() {
  local prefix="* "
  local git_branch_with_prefix=$(git branch)
  local git_branch=${git_branch_with_prefix#"$prefix"}
  echo "$git_branch"
}

function commitTagPush(){

  printf "\nChanging directory to %s\n" "$1"
  cd "$1"

  local git_main_branch="$2"

  git status
  git branch

  printf "\nRunning command: mvn clean install\n"
  local BUILD_SUCCESS=$(mvn clean install && echo "YES" || echo "NO")

  if [ "$BUILD_SUCCESS" = "NO" ]; then
    printf "\nExiting\n"
    exit 1
  else
    printf "\nBUILD SUCCESS\n"
  fi

  printf "\n%s\n" "$1"
  printYesNoPrompt "Proceed with adding all, commit and push?"

  if [ -z "$NEW_VERSION" ]; then
    # FOR LINUX USE  sed -i -e 's/abc/XYZ/g' "pom.xml"
    sed -i '' "s/rate-limiter.version>.\{1,\}</rate-limiter.version>$NEW_VERSION</" "pom.xml"
  fi

  local git_branch=$(getGitCurrentBranchName)
  local changed_branch=false

  if [ -z "$COMMIT_MESSAGE" ]; then
    printf "\nNo commit message was set - skipping commit\n";
  else
    git add .
    git commit -m "$COMMIT_MESSAGE"
    git push --set-upstream origin "$git_branch"
    if [ "$git_branch" != "$git_main_branch" ]; then
      changed_branch=true
      git checkout "$git_main_branch"
      git merge "$git_branch"
      git push --set-upstream origin "$git_main_branch"
    fi
  fi

  if [ -z "$NEW_VERSION" ]; then
    printf "\nNo new version was set - skipping tag\n";
  else
    git tag "v$NEW_VERSION"
    git push --tags
    if [ "$changed_branch" = true ]; then
      git checkout "$git_branch"
    fi
  fi
}

printf "\nEnter commit message  - If you enter nothing, no commit will be made.\n"

read COMMIT_MESSAGE

printf "\nEnter new version e.g 0.0.8  - If you enter nothing, no tag will be applied.\n"

read NEW_VERSION

commitTagPush "$HOME/Documents/dev/java/rate-limiter" "master"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-annotation" "main"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-web-core" "master"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-javaee" "master"
commitTagPush "$HOME/Documents/dev/java/rate-limiter-spring" "master"

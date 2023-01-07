#!/bin/bash

PARENT_DIR="$HOME/dev_looseboxes"
MVN_SETTINGS_FILE="/$HOME/dev_looseboxes/.m2/settings.xml"
UPDATE_CHANGE_LOG=false

function promptOkExit() {
  printf "\n%s\n" "$1"
  printf "\nEnter 1 for Yes, 2 for Exit\n"
  select yn in "Yes" "Exit"; do
      case $yn in
          Yes ) break;;
          Exit ) exit;;
      esac
  done
}

function getGitCurrentBranchName() {
  local prefix="* "
  local git_branch_with_prefix=$(git branch | grep "$prefix")
  local git_branch=${git_branch_with_prefix#"$prefix"}
  echo "$git_branch"
}

function insertTextAtLineInChangelog() {
  printf '%s\n' "$1" a "$2" . wq | ed CHANGELOG.md
}

function updateChangelog() {
    local current_date=$(date '+%Y-%m-%d')
    insertTextAtLineInChangelog 10 "
## [ [$NEW_VERSION](https://github.com/poshjosh/$1/tree/$NEW_VERSION) ] - $current_date

### Added

- $COMMIT_MESSAGE"
}

function commitTagPush(){

  printf "\nEnter project name e.g (rate-limiter) - Will exit if nothing is entered.\n"

  read project_name

  local project_dir="$PARENT_DIR/$project_name"

  printf "\nChanging directory to %s\n" "$project_dir"
  cd "$project_dir" || exit 1

  local git_main_branch="$1"

  git status
  git branch

  printf "\nRunning command: mvn clean install\n"
  local BUILD_SUCCESS=$(mvn clean install -s "$MVN_SETTINGS_FILE" && echo "YES" || echo "NO")

  if [ "$BUILD_SUCCESS" = "NO" ]; then
    printf "\nExiting\n"
    exit 1
  else
    printf "\nBUILD SUCCESS\n"
  fi

  printf "\n%s\n" "$project_name"

  if [ -z "$NEW_VERSION" ]; then
    printf "\nNo new version was set - skipping pom update for: %s\n" "$project_name"
  else

    promptOkExit "Proceed with version update to $NEW_VERSION?"

    # FOR LINUX USE  sed -i -e 's/abc/XYZ/g' "pom.xml"
    sed -i '' "s/rate-limiter.version>.\{1,\}</rate-limiter.version>$NEW_VERSION</" "pom.xml"

    printf "\nPrinting updated version from pom.xml\n"
    cat pom.xml | grep "rate-limiter.version>$NEW_VERSION<"

    updateChangelog "$project_name"

    printf "\nPrinting updated version from CHANGELOG.md\n"
    cat CHANGELOG.md | grep "$NEW_VERSION"
  fi

  local git_branch=$(getGitCurrentBranchName)
  printf "\nBranches. main: %s, current: %s\n" "$git_main_branch" "$git_branch"

  local changed_branch=false

  if [ -z "$COMMIT_MESSAGE" ]; then
    printf "\nNo commit message was set - skipping commit\n";
  else
    promptOkExit "Proceed with add, commit and push of $project_name?"
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

printf "\nEnter new version e.g 0.1 (will lead to tag v0.1) - If you enter nothing, no tag will be applied.\n"

read NEW_VERSION

function promptUpdateChangeLog() {
  printf "\n%s\n" "Update change log?"
  printf "\nEnter 1 for Yes, 2 for No\n"
  select yn in "Yes" "No"; do
      case $yn in
          Yes ) UPDATE_CHANGE_LOG=true; break;;
          No ) UPDATE_CHANGE_LOG=false; break;;
      esac
  done
}

promptUpdateChangeLog

commitTagPush "master"
commitTagPush "main"
commitTagPush "master"
commitTagPush "master"
commitTagPush "master"

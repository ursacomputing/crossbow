#!/bin/sh

# Copy of https://gist.github.com/antonio/4586456
# With a modification to collect all delete the branches in batches
# Set DRY_RUN=1 to get an echo of the command

# Format that works with `git log --since`, e.g. 2018-01-01
date=$1

while true; do

tags=
branches=

for branch in $(git branch -a --sort authordate | sed 's/^\s*//' | sed 's/^remotes\///' | grep -v 'master$' | head -n 50); do
  if [[ "$(git log $branch --since "${date}" | wc -l)" -eq 0 ]]; then
    echo $branch
    if [[ "$branch" =~ "origin/" ]]; then
      name=$(echo "${branch}" | sed 's/^origin\///')
      tag_ref="refs/tags/${name}"
      branch_ref="refs/heads/${name}"
      if [[ -z $branches ]]; then
        tags=$tag_ref
        branches=$branch_ref
      else
        tags="$tags $tag_ref"
        branches="$branches $branch_ref"
      fi
    fi
  else
    break
  fi
done

echo

if [[ ! -z $branches ]]; then
  if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "\nDelete remote branches:"
    echo git push --delete origin $branches
    echo
    echo "\nDelete remote tags:"
    echo git push --delete origin $tags
  else
    echo "\nDelete remote branches: $branches..."
    git push --delete --force origin $branches
    echo "\nDelete remote tags: $tags..."
    git push --delete --force origin $tags

    # clean up locally
    git remote prune origin
  fi
else
  break
fi

done

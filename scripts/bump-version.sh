#!/bin/bash

# Get the most recent tag
TAG=$1

# Remove 'v' prefix if present
VERSION=$(echo "$TAG" | sed 's/^v//')

# Update the Homebrew formula
sed -i.bak "s/version \".*\"/version \"$VERSION\"/g" shorm.rb
sed -i.bak "s/version: \".*\"/version: \"$VERSION\"/g" package.yaml
sed -i.bak "s/version_name = \".*\"/version_name = \"$VERSION\"/g" src/Shorm/Commands.hs

# Remove the backup files created by sed
rm shorm.rb.bak package.yaml.bak src/Shorm/Commands.hs.bak

# Stage the changed files
git add shorm.rb package.yaml src/Shorm/Commands.hs

# Commit the changes
git commit -m "Bump version to $VERSION"

# Amend the tag to include this new commit
git tag -f "$TAG"

echo "Updated Homebrew formula, and Commands.hs to version $VERSION"

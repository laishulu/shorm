#!/bin/bash

# Get the most recent tag
TAG=$(git describe --tags --abbrev=0)

# Remove 'v' prefix if present
VERSION=$(echo "$TAG" | sed 's/^v//')

# Function to download asset and calculate SHA256
download_and_hash() {
    local asset_name=$1
    local url="https://github.com/laishulu/shorm/releases/download/v${VERSION}/${asset_name}"
    local tmp_file="/tmp/${asset_name}"
    
    # Download the asset
    curl -L -o "$tmp_file" "$url"
    
    # Calculate SHA256
    if [[ "$OSTYPE" == "darwin"* ]]; then
        sha256=$(shasum -a 256 "$tmp_file" | awk '{print $1}')
    else
        sha256=$(sha256sum "$tmp_file" | awk '{print $1}')
    fi
    
    # Clean up
    rm "$tmp_file"
    
    echo "$sha256"
}

# Calculate SHA256 for each asset
macos_arm64_sha256=$(download_and_hash "shorm-macos-arm64")
macos_x86_64_sha256=$(download_and_hash "shorm-macos-x86_64")
linux_x86_64_sha256=$(download_and_hash "shorm-linux-x86_64")

# Update the Homebrew formula with actual SHA256 hashes and version
sed -i.bak \
    -e "s/version \".*\"/version \"$VERSION\"/" \
    -e "/url.*shorm-macos-arm64/{ n; s/sha256 \"[-_ a-zA-Z0-9]*\"/sha256 \"$macos_arm64_sha256\"/; }" \
    -e "/url.*shorm-macos-x86_64/{ n; s/sha256 \"[-_ a-zA-Z0-9]*\"/sha256 \"$macos_x86_64_sha256\"/; }" \
    -e "/url.*shorm-linux-x86_64/{ n; s/sha256 \"[-_ a-zA-Z0-9]*\"/sha256 \"$linux_x86_64_sha256\"/; }" \
    shorm.rb

# Remove the backup file created by sed
rm shorm.rb.bak

echo "Updated Homebrew formula with actual SHA256 hashes"
echo "Please review the changes, commit, and push them to GitHub"

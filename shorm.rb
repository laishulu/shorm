class Shorm < Formula
  desc "SSH Connection Manager"
  homepage "https://github.com/laishulu/shorm"
  version "1.0.0"

  if OS.mac?
    if Hardware::CPU.arm?
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-arm64"
      sha256 "c36e31d2c66fbca0107dac41557cbebbf88d40830dde339505112771b82bd0ed"
    else
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-x86_64"
      sha256 "501032f330412ff744961f9fd309d170cbe7646ec8c4368548bc4ab2848b99f7"
    end
  elsif OS.linux?
    url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-linux-x86_64"
    sha256 "eb9e21c2f5db244d61a9042833929113486d9cda4c8ae11f6062dd1d52fb4912"
  end

  def install
    bin.install "shorm-macos-arm64" => "shorm" if OS.mac? && Hardware::CPU.arm?
    bin.install "shorm-macos-x86_64" => "shorm" if OS.mac? && !Hardware::CPU.arm?
    bin.install "shorm-linux-x86_64" => "shorm" if OS.linux?
  end

  test do
    system "#{bin}/shorm", "--version"
  end
end

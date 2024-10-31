class Shorm < Formula
  desc "SSH Connection Manager"
  homepage "https://github.com/laishulu/shorm"
  version "1.0.0"

  if OS.mac?
    if Hardware::CPU.arm?
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-arm64"
      sha256 "d46475f569523fb0f3b375d0036f7a455a96aa2128de8b71d78a3cac672f9f17"
    else
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-x86_64"
      sha256 "273b518e624bac6cc75b35959f6248a3cc428fce211238108d0a82c2fea80524"
    end
  elsif OS.linux?
    url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-linux-x86_64"
    sha256 "9ab0cfb4748d14fa7ff430a66a41fbfe30164096d7a9f93786bed8dd6d24f2f5"
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

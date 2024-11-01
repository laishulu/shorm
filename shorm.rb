class Shorm < Formula
  desc "SSH Connection Manager"
  homepage "https://github.com/laishulu/shorm"
  version "1.1.1"

  if OS.mac?
    if Hardware::CPU.arm?
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-arm64"
      sha256 "8d3a22214b7c96dca572bd98d7387f7f8e85090cd7a428e94c034344ab478f50"
    else
      url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-macos-x86_64"
      sha256 "412c6460d259380a42329d73ba7e9f4823cf5a017bdb8bd5b9427b0eb5130af1"
    end
  elsif OS.linux?
    url "https://github.com/laishulu/shorm/releases/download/v#{version}/shorm-linux-x86_64"
    sha256 "ca00df6d98ccb2697849a0fe305184bb8b545e0a382dc966a64c71a4cefdccbe"
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

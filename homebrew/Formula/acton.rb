class Acton < Formula
  desc "Delightful distributed programming language"
  homepage "https://www.acton-lang.org"
  url "https://github.com/actonlang/acton/archive/refs/tags/v0.0.0.tar.gz"
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  license "BSD-3-Clause"
  head "https://github.com/actonlang/acton.git", branch: "main"

  depends_on "ghc@8.10" => :build
  depends_on "haskell-stack" => :build
  depends_on "libgc" => :build
  depends_on "libuv" => :build
  depends_on "pkg-config" => :build
  depends_on "protobuf-c" => :build
  depends_on "utf8proc" => :build

  on_macos do
    depends_on "argp-standalone" => :build
  end

  on_linux do
    depends_on "libbsd" => :build
    depends_on "gcc"
    depends_on "gmp"
    depends_on "util-linux"
  end

  def install
    # Fix up stack config to not install project local GHC
    inreplace "compiler/stack.yaml", "# system-ghc: true", <<~EOS
      system-ghc: true
      install-ghc: false
      allow-newer: true
    EOS

    ENV["BUILD_RELEASE"] = "1"
    system "make"
    bin.install "dist/bin/actonc"
    bin.install "dist/bin/actondb"
    bin.install "dist/bin/runacton"
    prefix.install Dir["dist/*"]
    bash_completion.install "completion/acton.bash-completion"
  end

  test do
    testapp = (testpath/"hello.act")
    testapp.write <<~EOS
      #!/usr/bin/env runacton
      actor main(env):
          print("Hello World!")
          await async env.exit(0)
    EOS
    testapp.chmod 0755
    assert_equal "Hello World!\n", shell_output(testapp)
  end
end

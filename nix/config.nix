{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "4c4c42e6230df599ea0b98ad5b47f5b2badb16e2";
    sha256 = "193fi4zbcr2qgkqg3kdl9w9sji0xvcnnikxaqxbys5avsfgqkqp9";
  };

in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}

* **[rocq-gameboy][1]**: a GameBoy emulator in Rocq. This is a work in progress,
  but includes an assember, disassembler, and a partial specification
  of the CPU using [Interaction Trees][itrees].

* **[stt][2]**: A toy language that uses set-theoretic types. This was an
  exploration of the type system described in [Covariance and
  Controvariance: a fresh look at an old issue][castagna].

* **[blog][3]**: The static site generator that powers this website. It's
  written in Haskell, and is built on to of [Shake][shake] and
  [Pandoc][pandoc].

[1]: https://github.com/jtmcx/rocq-gameboy
[2]: https://github.com/jtmcx/stt
[3]: https://github.com/jtmcx/blog

[itrees]: https://dl.acm.org/doi/10.1145/3371119
[coqasm]: https://dl.acm.org/doi/10.1145/2505879.2505897
[castagna]: https://arxiv.org/abs/1809.01427
[shake]: https://shakebuild.com/
[pandoc]: https://pandoc.org/
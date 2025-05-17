
# Notes on Unison Programming Language

The big idea is to store function abstract syntax tree (AST) as (SHA-3 512 bit)
hashes. This makes references unique and local naming independent.

What does this big idea solve?

As dependencies no longer rely on naming, it versions itself. If I change code
that it does something different, the hash will change. Function calls rely on
hash references, rather than names so if that changes it's part of the hash
update. If only naming changes, it has no effect. Thought experiment: Think
about those magical reproducible builds (chain of trust).

It particularly simplifies distributed computing. Provide the function hash and
input arguments to a remote endpoint, you're done.

> Unison can be viewed as a descendant of Haskell [...]
>   - Unison FAQs


[Unison Lang Documentation] contains guides, references and resources.

The codebase is a immutable data structure.

`'` in function signature indicates a delayed computation (via abilities).

Tests report cached results until hash changes.

[Unison.Cloud], hosting of Unison applications. Our approach page has a nice
example of how dependencies are avoided at deployments.

```
# from code view
factorial n = product (range 1 n)
myService = ... (factorial 9) ...
# turns into
factorial#a82j n = product#92df (range#b283 1 n)
myService = ... (factorial#a82j 9) ...
```

Unison.Cloud states its advantages with: Deploy with a function call, service
calls with one line of code, and use storage as simple as typed in-memory
variables. No encoders, decoders are needed (think of protobuf at grpc).


**Unison Code Manager (UCM)** is the development & runtime environment.

No code versioning as you upload functions, instead, UCM comes with it's [own
versioning system], and [in-code doc] supports code execution (examples),
mermaid diagrams and latex.

There is a [Unison Track at Exercism] available.

## Open Questions

How do I profit from bug or security fixes? Using dependencies, I'll retrieve
those for free, right? How does Unison handle this?

How do you collaborate in a team!?

[Unison Lang Documentation]: https://www.unison-lang.org/docs/ "Unison Documentation"
[Unison Track at Exercism]: https://exercism.org/tracks/unison "Unison at Exercism"
[Unison.Cloud]: https://www.unison.cloud/our-approach/ "Unison Cloud"
[in-code doc]: https://www.unison-lang.org/docs/usage-topics/documentation/ "Code Documentation"
[own versioning system]: https://www.unison-lang.org/docs/usage-topics/resetting-codebase-state/ "Codebase State"

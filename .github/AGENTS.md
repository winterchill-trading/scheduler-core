# .github/

`workflows/ci.yml`: `ox-setup` from the infra repository (cached opam switch), then
`dune build`, `dune test`, `dune build @examples/runtest`, and the reusable obj-magic
check. The repo has no org dependencies, so there is no `pins:` list; if one is added to
`dune-project`, add it to `pins:` in dependency order. Check runs with
`gh run list --limit 1` and at most one `gh run watch`.

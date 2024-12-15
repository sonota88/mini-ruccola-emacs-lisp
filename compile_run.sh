#!/bin/bash

readonly RUNNER_CMD="emacs --script"

compile() {
  $RUNNER_CMD mrcl_lexer.el \
    | $RUNNER_CMD mrcl_parser.el \
    | $RUNNER_CMD mrcl_codegen.el \
    | $RUNNER_CMD mrcl_asm.el
}

container_main() {
  local src_file="$1"; shift

  mkdir -p z_tmp
  local exe_file="z_tmp/temp.exe.txt"

  cat $src_file | compile > $exe_file

  EXEFILE=$exe_file \
    emacs --no-window-system --no-splash --load mrcl_vm.el
}

in_container() {
  env | grep --quiet IN_CONTAINER
}

if (in_container); then
  container_main "$@"
else
  # Run in container
  ./docker.sh run bash compile_run.sh "$@"
fi

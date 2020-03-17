#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.1:2.0.4 \
  org.typelevel:cats-core_2.13:2.1.1 \
  io.scalaland:chimney_2.13:0.5.0 \
  io.scalaland:chimney-cats_2.13:0.5.0 \
  -- --predef-code 'import io.scalaland.chimney.dsl._;import io.scalaland.chimney.cats._' < /dev/tty

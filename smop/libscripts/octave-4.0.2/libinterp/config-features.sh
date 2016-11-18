#! /bin/sh

set -e
AWK=${AWK:-awk}

conffile=$1

cat << EOF
// DO NOT EDIT!  Generated automatically from $conffile by Make."

#include "oct-map.h"
#include "ov.h"

octave_scalar_map
octave_config_features (void)
{
  octave_scalar_map m;

EOF

$AWK \
  '/#define HAVE_/ {
     sub (/HAVE_/, "", $2);
     printf ("  m.assign (\"%s\", octave_value (true));\n", $2);
   }
   /\/\* #undef HAVE_/ {
     sub (/HAVE_/, "", $3);
     printf ("  m.assign (\"%s\", octave_value (false));\n", $3);
   } {
   }' $conffile

cat << EOF

  return m;
}
EOF

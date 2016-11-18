#! /bin/sh

set -e

: ${AWK=awk}

if [ $# -lt 3 ]; then
  src_type="all"
else
  src_type="$3"
fi

if [ $# -lt 2 ]; then
  obj_type="all"
else
  obj_type="$2"
fi

if [ $# -lt 1 ]; then
  top_srcdir="../.."
else
  top_srcdir="$1"
fi

move_if_change="$top_srcdir/build-aux/move-if-change"

liboctave_dir="$top_srcdir/liboctave/operators"

mk_ops="$liboctave_dir/mk-ops.awk"
sparse_mk_ops="$liboctave_dir/sparse-mk-ops.awk"

case "$obj_type" in
  vx | all)
    case "$src_type" in
      inc | all)
        VX_INC=$($AWK -f $mk_ops prefix=vx list_h_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_INC = $VX_INC" > $liboctave_dir/vx-op-inc.mk-t
        $move_if_change $liboctave_dir/vx-op-inc.mk-t $liboctave_dir/vx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  vx | all)
    case "$src_type" in
      src | all)
        VX_SRC=$($AWK -f $mk_ops prefix=vx list_cc_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_SRC = $VX_SRC" > $liboctave_dir/vx-op-src.mk-t
        $move_if_change $liboctave_dir/vx-op-src.mk-t $liboctave_dir/vx-op-src.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      inc | all)
        MX_INC=$($AWK -f $mk_ops prefix=mx list_h_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_INC = $MX_INC" > $liboctave_dir/mx-op-inc.mk-t
        $move_if_change $liboctave_dir/mx-op-inc.mk-t $liboctave_dir/mx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      src | all)
        MX_SRC=$($AWK -f $mk_ops prefix=mx list_cc_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_SRC = $MX_SRC" > $liboctave_dir/mx-op-src.mk-t
        $move_if_change $liboctave_dir/mx-op-src.mk-t $liboctave_dir/mx-op-src.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      inc | all)
        SMX_INC=$($AWK -f $sparse_mk_ops prefix=smx list_h_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_INC = $SMX_INC" > $liboctave_dir/smx-op-inc.mk-t
        $move_if_change $liboctave_dir/smx-op-inc.mk-t $liboctave_dir/smx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      src | all)
        SMX_SRC=$($AWK -f $sparse_mk_ops prefix=smx list_cc_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_SRC = $SMX_SRC" > $liboctave_dir/smx-op-src.mk-t
        $move_if_change $liboctave_dir/smx-op-src.mk-t $liboctave_dir/smx-op-src.mk
      ;;
    esac
  ;;
esac






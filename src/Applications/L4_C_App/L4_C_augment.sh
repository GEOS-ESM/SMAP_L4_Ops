#!/bin/sh

# Retrieve Arguments
# ==================

  if [ $# -ne 2 ]; then
    echo "Usage: $0 [input name] [output name]"
    exit 1
  fi

  iname=$1
  oname=$2

# Set up Python Environment
# =========================

  export L4_C_AUGMENT=1

  source $SMAP_HOME/g5_modules_sh
  module purge
  source $SMAP_HOME/g5_modules_sh
  module list

# Execute the Augmentation Software
# =================================

  L4_C_augment.py $1 $2

  if [ $? -ne 0 ]; then
    exit 2
  fi

exit 0

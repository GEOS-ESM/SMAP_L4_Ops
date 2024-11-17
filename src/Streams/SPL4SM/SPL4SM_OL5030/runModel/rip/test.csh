#!/bin/csh

foreach t ( 0130 0430 0730 1030 1330 )

  cp CURRENT.SMAP_L4_SM_gph.20190101_${t}z.bin <L4_SM_MODEL_OUTPUT_DIR>/cat/ens_avg/Y%Y/M%m/<L4_SM_STREAM_version>.ens_avg.ldas_tile_xhourly_out.20190101__${t}z.bin

end

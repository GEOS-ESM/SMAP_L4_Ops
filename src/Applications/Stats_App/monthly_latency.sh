#!/bin/sh


for mm in 01 02 03 04 05 06 07 08 09 10 11 12; do

# /bin/ls PAN/Y2025/SPL4CMDL.2025${mm}*.PAN | sort | ./latency.py
  /bin/ls PAN/Y2025/SPL4SMGP.2025${mm}*T0130*.PAN | sort | ./latency.py

done

exit 0

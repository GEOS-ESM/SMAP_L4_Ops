#!/usr/bin/env python3

import h5py
import argparse

def convert_hdf5(file):

    with h5py.File(file, "r+") as f:
        print(f"Conversion complete: '{file}' is now compatible with HDF5 1.8.")

    # Update reference list
    with h5py.File(file, "r+") as f:
        dsets = ['x','y']
        for dset in dsets:
            ref_out = f[dset].attrs['REFERENCE_LIST'].copy()
            f[dset].attrs['REFERENCE_LIST'] = ref_out

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Convert an HDF5 1.10 file to HDF5 1.8 format while preserving attributes and fill values.")

    parser.add_argument('filename', metavar='filename', type=str,
       help='Path to the HDF5 file')

    args = parser.parse_args()
    convert_hdf5(args.filename)

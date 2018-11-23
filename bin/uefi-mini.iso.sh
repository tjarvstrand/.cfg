#!/usr/bin/env bash



if [ "$1" == "" ]
then
    echo "Must give a mini.iso file as argument"
    exit 1
fi

set -ex

DIR=/tmp/mini.iso.WhgacvL6
#DIR=$(mktemp --tmpdir -d mini.iso.XXXXXXXX)

MINI_ISO_DIR=${DIR}/mini.iso
EFI_IMG_DIR=${DIR}/efi.img
OUTPUT_DIR=${DIR}/mini

mkdir -p ${MINI_ISO_DIR}

mount -o loop $1 ${MINI_ISO_DIR}
cp -r ${MINI_ISO_DIR} ${OUTPUT_DIR}

umount ${MINI_ISO_DIR}
rmdir ${MINI_ISO_DIR}

mkdir -p ${EFI_IMG_DIR}
LOOP_DEVICE=$(losetup --show -f -P ${OUTPUT_DIR}/boot/grub/efi.img)
#LOOP_DEVICE=$(losetup -n -ONAME -j ${OUTPUT_DIR}/boot/grub/efi.img)
mount ${LOOP_DEVICE} ${EFI_IMG_DIR}

cp -r ${EFI_IMG_DIR}/efi ${OUTPUT_DIR}

umount ${EFI_IMG_DIR}
losetup -d ${LOOP_DEVICE}
rmdir ${EFI_IMG_DIR}

echo
echo "-------------------------------------------------------------------------"
echo
echo "You can now copy ${EFI_IMG_DIR} to fat32-formatted device!"
echo
echo "-------------------------------------------------------------------------"


#!/bin/bash

##### INFALTE REAL WORLD IMAGES AND CSVs

# Define paths
DATASET_DIR="./realWorld_data_processing/realWorld_datasets/tables"
IMAGES_DEST="$DATASET_DIR/images"
CSVS_DEST="$DATASET_DIR/csvs"

# Create destination folders if they don't exist
mkdir -p "$IMAGES_DEST" "$CSVS_DEST"

# Extract all images_pt*.tar.gz files and move contents to images folder
for file in "$DATASET_DIR"/images_pt*.tar.gz; do
    tar -xzf "$file" -C "$DATASET_DIR"
done

# Move all extracted images into the images directory
find "$DATASET_DIR" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" \) -exec mv {} "$IMAGES_DEST" \;

# Optional: Remove now-empty extracted image folders
find "$DATASET_DIR" -mindepth 1 -type d -name "images_pt*" -exec rm -r {} +

# Extract tables.csvs.tar.gz into csvs folder
tar -xzf "$DATASET_DIR/tables.csvs.tar.gz" -C "$CSVS_DEST"

echo "All images extracted and moved to $IMAGES_DEST"
echo "tables.csvs.tar.gz extracted into $CSVS_DEST"

##### 
pushd images
bmp2tile.exe "graphics.png" -savetiles "graphics.png.tiles.zx7" -savepalette "palette.bin"
bmp2tile.exe "graphics.png" -savetilemap "graphics.png.tilemap.bin"
for %%f in (tiles*.png) do bmp2tile.exe "%%f" -tileoffset 1 -savetiles "%%f.tiles.zx7"
popd
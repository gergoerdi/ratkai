all: game/bosszu/full/text1.bin.zx0 game/bosszu/full/text2.bin.zx0 game/bosszu/full/pics-tvc.bin.zx0

game/bosszu/full/%.bin.zx0: _obj/bosszu.tvc/%.bin
	./import/ZX0/src/zx0.exe -b -f $< && mv -f $<.zx0 $@

_obj/bosszu.tvc/text1.bin _obj/bosszu.tvc/text2.bin: game/bosszu/full/text1.txt game/bosszu/full/text2.txt
	stack run ratbc-tvc-asm -- -b -i game/bosszu/full/ -o _obj/bosszu.tvc/


#OBJS specifies which files to compile as part of the project
OBJS = loaders.c game.c main.c

#OBJ_NAME specifies the name of our exectuable
OBJ_NAME = out

#This is the target that compiles our executable
all : $(OBJS)
	ghc --make -O DAWG.hs
	ghc --make -no-hs-main -optc-O $(OBJS) DAWG -I./SDL2 -w -g -lm -lSDL2main -lSDL2 -o game

clean:
	-rm *.hi *.o game

# clash --make -O FFI.hs -i../rv32i/
# 

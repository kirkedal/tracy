FILES=Main.hs Tracy/*.hs
MAIN=Main.hs
NAME=Tracer

all: main

warn, w: $(FILES)
	ghc -XGeneralizedNewtypeDeriving -XDeriveDataTypeable -Wall -fno-warn-unused-do-bind -o $(NAME) --make $(MAIN)
	mv Main $(NAME)

main: $(FILES)
	ghc -XGeneralizedNewtypeDeriving -XDeriveDataTypeable -O2 -rtsopts -o $(NAME) --make $(MAIN)

debug: $(FILES) 
	ghc -XGeneralizedNewtypeDeriving -XDeriveDataTypeable -o $(NAME) --make $(MAIN)

haddock, h: $(FILES)
	haddock -h -o ../docs/ $(MAIN)

clean:
	-rm -f $(NAME)
	-rm -f *.hi *.o 
	-rm -f Tracy/*.hi Tracy/*.o


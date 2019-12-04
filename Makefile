##
## EPITECH PROJECT, 2019
## deBruijn
## File description:
## Makefile
##

NAME = imageCompressor

SRC =	app/Main.hs
		# src/parser.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .

clean:
	stack clean
	rm .stack-work image-compressor.cabal -rf

fclean:	clean
	# rm imageCompressor
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re

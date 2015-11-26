#!/bin/bash

L_BLUE='\033[1;34m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

printf "\n${L_BLUE}\tCompiling TYPES\n"

# types
printf "\n${GREEN}compiling ${CYAN}coconuts${NC}\n"
cs3110 compile src/types/coconuts.ml

printf "\n${L_BLUE}\tCompiling CAMEL\n"

# camel
printf "\n${GREEN}compiling ${CYAN}camel.ml${NC}\n"
cs3110 compile src/camel/camel -I src/types/

printf "\n${L_BLUE}\tCompiling OASYS\n"

# oasys
printf "\n${GREEN}compiling ${CYAN}fileio${NC}\n"
cs3110 compile src/oasys/fileio

printf "\n${GREEN}compiling ${CYAN}cameljson.ml${NC}\n"
cs3110 compile src/oasys/cameljson -I src/types/

printf "\n${GREEN}compiling ${CYAN}palmtreeupdater.ml${NC}\n"
cs3110 compile src/oasys/palmtreeupdater -I src/types/

printf "\n${GREEN}compiling ${CYAN}oasys.ml${NC}\n"
cs3110 compile src/oasys/oasys -I src/types/

printf "\n${L_BLUE}\tCompiling CAMEL RIDER\n"

# camelrider
printf "\n${GREEN}compiling ${CYAN}camelrider.ml${NC}\n"
cs3110 compile src/camelrider/camelrider -I src/types/ -I src/oasys/

printf "\n${L_BLUE}\tCompiling MAIN\n"

# main
printf "\n${GREEN}compiling ${CYAN}main.ml${NC}\n"
cs3110 compile src/main.ml -I src/types/ -I src/oasys/ -I src/camel/


printf "\n${L_BLUE}\tDone.${NC}\n\n"

#!/bin/bash

L_BLUE='\033[1;34m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

printf "\n\n${L_BLUE}\tCompiling TYPES\n\n"

# types
printf "\n${GREEN}compiling ${CYAN}coconuts${NC}\n"
cs3110 compile src/types/coconuts

printf "\n\n${L_BLUE}\tCompiling CAMEL\n\n"

# camel
printf "\n${GREEN}compiling ${CYAN}camel.ml${NC}\n"
cs3110 compile src/camel/camel -I src/types/

printf "\n${L_BLUE}\n\n\tCompiling OASYS\n\n"

# oasys
printf "\n${GREEN}compiling ${CYAN}fileio${NC}\n"
cs3110 compile src/oasys/fileio

printf "\n${GREEN}compiling ${CYAN}cameljson.ml${NC}\n"
cs3110 compile src/oasys/cameljson -I src/types/

printf "\n${GREEN}compiling ${CYAN}palmtreeupdater.ml${NC}\n"
cs3110 compile src/oasys/palmtreeupdater -I src/types/

printf "\n${GREEN}compiling ${CYAN}oasys.ml${NC}\n"
cs3110 compile src/oasys/oasys -I src/types/

printf "\n\n${L_BLUE}\tCompiling CAMEL RIDER\n\n"

# camelrider
printf "\n${GREEN}compiling ${CYAN}camelrider.ml${NC}\n"
cs3110 compile src/camelrider/camelrider -I src/types/ -I src/oasys/

printf "\n\n${L_BLUE}\tdone.${NC}\n\n"

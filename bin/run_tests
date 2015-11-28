#!/bin/bash

L_BLUE='\033[1;34m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

printf "\n${L_BLUE}Starting tests...${NC}\n"
printf "${GREEN}Testing: ${CYAN}palmtreeupdater${NC}\n"
cs3110 compile test/oasys/palmtreeupdater_test -I src/util/ -I src/types/ -I src/oasys/
cs3110 test test/oasys/palmtreeupdater_test
printf "${L_BLUE}Done.${NC}\n"
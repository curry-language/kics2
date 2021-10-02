#!/bin/sh
# Output a colored success message

echo "$(tput bold)$(tput setaf 2)==> $@$(tput sgr0)"

# -*- coding: utf-8 -*-

# $2
module ${1:`(replace-regexp-in-string "[^A-Za-z]" "" (upcase-initials (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))`}
  $0
end

if __FILE__ == \$PROGRAM_NAME
  # this will only run if the script was the main, not load'd or require'd
end

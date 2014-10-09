#!/usr/bin/env bash

gen_filenametags()
{
    rm -f filenametags
    echo -e "!_TAG_FILE_SORTED\t2\t/2=foldcase/" > filenametags
    # find . -not -regex '.*\.\(Plo\|lo\|o\|png\|gif\)' -type f -printf "%f\t%p\t1\n" | sort -f >> filenametags
    find $(pwd) -name '*.[he]rl' -type f -printf "%f\t%p\t1\n" | sort -f >> filenametags
}

gen_tags()
{
    rm -f tags
    ctags -R --fields=+lS .
}

gen_cscope()
{
    rm -f cscope*
    find $(pwd) -name "*.[he]rl" > cscope.files
    cscope -bq
}

gen_tags
gen_cscope

#!/bin/bash

# custom script to reduce tomfoolery on the interwebs
#####################################################

# list of sites to block
sites="reddit.com www.reddit.com news.ycombinator.com facebook.com www.facebook.com vimeo.com www.vimeo.com youtube.com www.youtube.com"

# redirect target
dest="127.0.0.1"

# essential line for toggle functionality
comment=" # JADB FUNCTION, DO NOT REMOVE OR ALTER"

# hosts file to alter
file="/etc/hosts"
                    
# altogether!
line="$dest  =  $sites  # JADB FUNCTION, DO NOT REMOVE OR ALTER"

# toggle time, if its already there...
if grep -Fxq "$line" $file
then
    # then get rid of it
    sed -i '/JADB/d' $file
    echo "...makes Jack a dull boy"
else
    # if not, then add it
    echo "$line" >> $file
    echo "All work and no play..."
fi

#!/bin/sh
iconv -f $1 -t $2 < $3 > /tmp/iconv.txt
./utftrip.native -ienc $1 -oenc $2 < $3 > /tmp/utftrip.txt
diff -a /tmp/iconv.txt /tmp/utftrip.txt




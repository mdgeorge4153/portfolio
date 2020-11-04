#! /bin/sh

usage() {
    echo "huffman.sh [filename]"
    echo "huffman.sh will compress and decompress a file."
}

if test $# -lt 1; then
    usage
    exit
fi

filename="$1"
base="$(basename $filename)"
cs3110 run compress.ml "$filename" > "$base.huff" && \
cs3110 run decompress.ml "$base.huff" > "$base.dehuff" && \
diff -s "$filename" "$base.dehuff"

#!/bin/bash

# 첫 번째 인자로 파일 경로가 적힌 텍스트 파일을 받음
file=$1

# 파일 내 각 라인에 대해 cat 명령어를 실행
while read line; do
    date
    echo "$line"
    filename=$(basename "$line")
    logfilename="$filename"".collection.txt"
    stack exec -- c11parser-exe "$line" &> "./kandr/""$logfilename"
    # for test
    # cat "$line" &> "$logfilename"
done < "$file"

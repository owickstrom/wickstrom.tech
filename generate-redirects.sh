
for file in `ls src/posts`; do
    new_path=$(echo $file | sed -E 's|(.*).md|/\1.html|')
    target=$(echo $file | sed -E 's|([0-9]{4})-([0-9]{2})-([0-9]{2})-(.*)\.md|src/redirects/programming/\1/\2/\3/\4.html|')
    mkdir -p $(dirname $target)
    cat << EOF > $target
    <!DOCTYPE html>
    <html>
    <head>
    <meta charset="utf-8">
    <title>Redirecting...</title>
    <link rel="canonical" href="https://wickstrom.tech$new_path">
    <meta http-equiv="refresh" content="0; url=https://wickstrom.tech$new_path">
    </head>
    </html>
EOF
done
# management
clean:
    cabal clean
    cabal run website clean
    rm -rf {{justfile_directory()}}/dist-newstyle

deep-clean: clean
    rm -rf ~/.cache/hie-bios \
           ~/.cache/ghcide \
           ~/.hie
    @echo "Clear and ready to go"

# development
build:
    cabal run website build

cleanbuild: clean build
    echo "Complete!"

watch:
    cabal run website watch

# deployment
publish message: clean build
    git add -A
    git commit -m "{{message}}"
    git push -u origin main

# writing
post_dir := justfile_directory() / "content/posts"
today    := `date +%Y-%m-%d`

new title:
    #!/bin/sh
    filename=$(printf '%s' "{{title}}" | tr '[:upper:]' '[:lower:]' | tr ' ' '-')
    file="{{post_dir}}/{{today}}-$filename.org"

    printf '%s\n' \
        '---' \
        'title: "{{title}}"' \
        'description: TODO' \
        'tags: TODO' \
        'toc: true' \
        'date: {{today}}' \
        'lastmod: {{today}}' \
        '---' \
        > "$file"

    echo "Created: $file"

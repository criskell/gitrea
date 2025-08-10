# gitrea

## running

```
docker build -t gitrea-ghc .
docker run -it --rm -v $(pwd):/workspace gitrea-ghc
cabal update
cabal install --only-dependencies --force-reinstalls
cabal configure
cabal build
./dist/build/gitrea/gitrea clone git://0.0.0.0/myrepo
```

## testing test-delta

git:
```
git clone git@github.com:git/git.git
make configure
./configure
make
./t/helper/test-tool delta <...>
```

app:
```
runhaskell -isrc ./src/Gitrea/Packfile/Delta.hs ./git-zlib.c zlib-delta
diff -q target.file git-zlib-changed.c
```

## git

```
git init myrepo.git --bare
git daemon --verbose --export-all --base-path=$(pwd) --reuseaddr --informative-errors --enable=receive-pack
git clone git://0.0.0.0/myrepo.git myrepo
cd myrepo
echo oi > example.txt
git add .
git commit -m "hello world"
git push origin master
```
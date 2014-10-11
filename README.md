Prototype conference review system.

# Install

```
cabal sandbox init
cabal add-source /path/to/hails
cabal install
./.cabal-sandbox/bin/lambdachair
```

## Usage

* User `root` is the PC chair. As this user you can add new PC members.

* As a PC member you can review papers. If you are in conflict with a paper you cannot view the reviews of other reviewers. You can still write a review/notes though.

* As a user you can upload papers.

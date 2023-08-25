# Update all GIT repos in directory

## Execute

With `stack exec -- gitallup --help` to see this text:

```
Update all GIT repos in directory

Usage: gitallup [--version] [--help] [-p|--path PATH] [-r|--recursive]
                [-d|--depth NUMBER] [-S|--status] [-C|--clean]
                [-D|--delete-branches] [-m|--main-branch] [-f|--force]
                [-o|--only LIST] [-x|--exclude LIST] [-a|--actions FILE]
                [-v|--verbose]

  Walks through all subdirectories of the given or current directory, and checks
  the status or performs git pull on all GIT repos

Available options:
  --version                Show version
  --help                   Show this help text
  -p,--path PATH           Path to directory where to update all existing GIT
                           repos (default: ".")
  -r,--recursive           Go recursively through subdirectories which are not
                           GIT repos?
  -d,--depth NUMBER        The depth of directory recursion (default: -1)
  -S,--status              Check status of the repositories.
  -C,--clean               Clean up repository.
  -D,--delete-branches     Delete branches that don't have a remote equivalent.
  -m,--main-branch         Switch all to main/default branch? Ignored when
                           --status/-S is passed as parameter.
  -f,--force               Force update overriding any local changes? Ignored
                           when --status/-S is passed as parameter.
  -o,--only LIST           List of directories/repositories to be selected,
                           comma separated.
  -x,--exclude LIST        List of directories/repositories to be excluded from
                           updating, comma separated.
  -a,--actions FILE        Path to the file that contains actions for select
                           repositories
  -v,--verbose             Verbose output?
```

# Update all GIT repos in directory

## Execute  

With `stack exec -- gitallup-exe --help` to see this text:
  ~~~~
  Update all GIT repos in directory

  Usage: gitallup [--version] [--help] [-p|--path PATH] [-r|--recursive]
                  [-d|--depth NUMBER] [-m|--master] [-v|--verbose]
    Walks through all subdirectories of the given or current directory, and
    performs git pull on all GIT repos

  Available options:
    --version                Show version
    --help                   Show this help text
    -p,--path PATH           Path to directory where to update all existing GIT
                             repos (default: ".")
    -r,--recursive           Go recursively through subdirectories which are not
                             GIT repos?
    -d,--depth NUMBER        The depth of directory recursion (default: -1)
    -m,--master              Switch all to master branch?
    -v,--verbose             Verbose output?  
  ~~~~  

## Run tests

`stack test`

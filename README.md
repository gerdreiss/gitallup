# Update all GIT repos in directory

## Execute  

With `stack exec -- gitallup-exe --help` to see this text:
  ~~~~
    Available options:
      --version                Show version
      --help                   Show this help text
      -d,--directory PATH      Root directory where to update all existing GIT repos (default: ".")
      -r,--recursive           Go recursively through subdirectories which are not GIT repos?
      -m,--master              Switch all to master branch?
      -v,--verbose             Verbose output?
  ~~~~  

## Run tests

`stack test`

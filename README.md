# Update all GIT repos in directory

## Execute  

* Run `stack exec -- gitallup-exe` to see "We're inside the application!"
* With `stack exec -- gitallup-exe --verbose` you will see the same message, with more logging.
* With `stack exec -- gitallup-exe --directory <path-to-workspace>` to update all GIT repos in 'path-to-workspace'
* With `stack exec -- gitallup-exe --help` to see this text:
  ~~~~
    Available options:
    --version                Show version
    --help                   Show this help text
    -d,--directory PATH      Root directory where to update all
                             existing GIT repos (default: ".")
    -m,--master              Switch all to master branch?
    -v,--verbose             Verbose output?
  ~~~~  

## Run tests

`stack test`

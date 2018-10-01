((f90-mode . ((compile-command . "cd build; cmake-gui .. && make clean && make rebuild_cache && make clean && make && make test || ctest --verbose; cd .."))))

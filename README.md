# mak

```
$ cat makfile
# actually importing not working yet
import "xalala"

# this is a constant
# another comment line
x = "value"

# this is a task
task taskname:
    yo = 123
    print(yo)
    print(x)

$ mak taskname
123
value
```

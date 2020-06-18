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

# Features

* Built-in functions with named arguments

# TODO

* String interpolation:
```
tast example:
    x = 123
    print("this is the value for x: #{x}")
```

* Support built-in functions with return values:
```
task example:
    x = ask(msg: "replicas: ", default: 1)
    print(x)
```

* More built-in functions (`ask` to get user input, what else?)

* Better integration with shellscript:
```
task example:
    image = k get deploy #{deployment} -o json | jq '.spec.template.spec.containers[] | select(.name == "#{deployment}") | .image' cut -d: -f2
    image = ask(msg: "New image for deploy: ", default: image)
    ktmpl kubernetes/manifests/deployment.yml -p IMAGE "#{image}" | k apply -f -
    print("Deployed!")
```

* Better parsing error messages

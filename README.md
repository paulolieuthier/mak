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
    z = ask(msg: "new value: ", default: "fallback")
    print("#{x}: #{z}")

$ mak taskname
123
value
new value: 456
456
```

# Features

* Built-in functions with named arguments and return types (`print` and `ask`)
* String interpolation

# TODO

* Better integration with shellscript:
```
task example:
    image = k get deploy #{deployment} -o json | jq '.spec.template.spec.containers[] | select(.name == "#{deployment}") | .image' cut -d: -f2
    image = ask(msg: "New image for deploy: ", default: image)
    ktmpl kubernetes/manifests/deployment.yml -p IMAGE "#{image}" | k apply -f -
    print("Deployed!")
```

* Better parsing error messages

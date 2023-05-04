# purescript-httpurple-om-example

A simple example to show how to use [yoga-om](https://github.com/rowtype-yoga/purescript-yoga-om) with HTTPurple 🪁.

```bash
spago build
spago run
```

```
➜ http GET http://localhost:8080/hello
HTTP/1.1 200 OK
Connection: keep-alive
Content-Length: 14
Date: Thu, 04 May 2023 06:56:37 GMT
Keep-Alive: timeout=5

hello John Doe
```

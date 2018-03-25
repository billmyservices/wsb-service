# wsb-service

See https://www.billmyservices.com

Simple *post* to billmyservice wrapper avoiding authentication encryption.

**WARNING** this service **open access** to your billmyservice counters! you must protect this service for unauthorized access.

## Example

Setup as daemon one automatic authenticated resource checker wrapper:
```
$ WSB_URL=http://services.billmyservices.com WSB_USERID=50 WSB_SKEY=M6UxiYsELKKHclwYFfKluzvuwj7Bvtk1pY5RUtPhUb4= WSB_PORT=9191 wsb-service
```

Now, you can update resources using:
```
$ curl http://localhost:9191/your-counter-type-code/your-counter-code/value
```


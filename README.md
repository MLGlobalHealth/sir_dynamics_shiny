# sir_dynamics_shiny

## Serve
This setup predominantly follows [this blog](https://www.charlesbordet.com/en/guide-shiny-aws/#how-to-set-up-your-new-ssltls-certificates-with-nginx)
with modifications to a. deploy each server within a docker-container and b. use CloudFlare as a proxy service.

### Build images
```
docker build -f serve/Dockerfile --target simple -t sir_dynamics:simple .
docker build -f serve/Dockerfile --target complicated -t sir_dynamics:complicated .
```

### SSL certs
Create SSL certificates with a provider, [CloudFlare](cloudflare.com) for example,
and copy them to the `serve/certs` folder.
This should include
* `cloudflare.crt`
* `hjtunwin.uk.crt`
* `hjtunwin.uk.key`

Download `serve/certs/dhparam.pem` with
```
curl https://ssl-config.mozilla.org/ffdhe2048.txt > serve/certs/dhparam.pem
```

### Set password

Create username/password with
```
echo -n 'niph' > serve/.htpasswd
openssl passwd -apr1 >> serve/.htpasswd
```
which will allow you to type the password interactively (only the hash is stored)


### Start server

Finally, start/stop the server images with
```
docker compose up -d
docker compose down
```
inside the `serve/` directory.

### Code updates
Build images:
```
docker build -f serve/Dockerfile --target simple -t sir_dynamics:simple .
docker build -f serve/Dockerfile --target complicated -t sir_dynamics:complicated .
```
from root directory.

Then from `serve/` directory, run `docker compose restart`

# middleman

A system for computing derivations on multiple machines. This project is different 
than hydra in that hydra supports continuous integration of a git repository. middleman
supports building arbirary derivations on a store, ready for download later.

Middleman consists of 4 possible separate machines: the server, the client, the
store, and the worker. There can be multiple clients and workers machines. 

The use case is, the client requires a derivation build, so it send a request to
the server. The server then distributes the builds to the workers which in turn
computes the derivation and push it back onto the store. The client can now
download the results from the store.

## Setup

### Creating the keys for the server and workers.

### The store. 

First setup the binary cache key: 

```
sudo nix-store --generate-binary-cache-key <name>-<number> /etc/nix/signing-key.sec /etc/nix/signing-key.pub
```

First setup the nix-server as a binary cache. 

```
networking = {
  ... 
  firewall = { 
    allowedTCPPorts = [ 22 5000 ];
  };
}

services = {
  ...
  # Enable ssh
  openssh.enable = true;
  
  # Enable nix-serve (binary-cache)
  nix-serve = { 
    enable = true;
    port = 5000;
    secretKeyFile = "/etc/nix/signing-key.sec"
  };
};
```

### Adding a worker 

A worker needs access to the binary cache, so it is important to 
add the binary cache:

```
nix = {
  binaryCaches = [
    "http://<store>:5000"
  ];
  binaryCachePublicKeys = [
    '<content of /etc/nix/signing-key.pub>'
  ];
};
```

Also we need to setup the ssh connection. We do this by generating a public 
key in the worker and then pushing it to the server: 

```
ssh-keygen
ssh-copy-id -i ~/.ssh/id_rsa.pub <user>@<store>
```

Now it should be possible to build any derivation in the store, and copy it 
back using:
```
nix copy --to ssh://<user>@<store> $(nix-store -r <the derivation>.drv)
```

# TODO

- Allow the client to wait for the process to be done
- Fix problem with add-time


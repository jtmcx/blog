---
title: Setting up Anubis with relayd(8) on OpenBSD
date: 2026-08-10
summary: |
  All the cool kids are using [Anubis] to block bots and AI scrapers. This
  is a basic tutorial on how to configure Anubis on an OpenBSD server with
  [relayd(8)] and [httpd(8)].
---

All the cool kids are using [Anubis] to block bots and AI scrapers. This
is a basic tutorial on how to configure Anubis on an OpenBSD server with
[relayd(8)] and [httpd(8)].

## Initial Setup

Before configuring Anubis, we start with a minimal [httpd(8)] and
[relayd(8)] setup. We configure relayd to listen on `egress:443`.
It terminates TLS and forwards unencrypted HTTP traffic to httpd on
`127.0.0.1:8080`. Relayd sets the headers `X-Forwarded-For` and
`X-Forwarded-By` in order to pass connection information along to httpd.
We assume that there is already an SSL certificate configured.

```
# /etc/relayd.conf

http protocol https {
    tls keypair jtm.cx
    match header set "X-Forwarded-For" \
        value "$REMOTE_ADDR"
    match header set "X-Forwarded-By" \
        value "$SERVER_ADDR:$SERVER_PORT"
}

relay "https" {
    listen on egress port 443 tls
    protocol "https"
    forward to 127.0.0.1 port 8080    # httpd
}
```

```
# /etc/httpd.conf

server "jtm.cx" {
    listen on localhost port 8080
    root "/htdocs/site"
    log style forwarded
}
```

Note the use of `log style forwarded`. By default, [httpd(8)] logs the
IP address of the client, but because we're listening on localhost,
the logs always show `127.0.0.1`. Setting `log style forwarded` appends
the value of `X-Forwarded-For` to the log line, allowing us to see the
client's IP. See [httpd.conf(5)] for more information.

Now we start relayd and httpd.

```
# rcctl enable relayd httpd
# rcctl start relayd httpd
```

## Installing Anubis

Now we can set up Anubis. Anubis is an HTTP proxy; it will live between
relayd and httpd. Relayd will still be responsible for accepting public
connections and terminating TLS, but instead of forwarding HTTP traffic
directly to httpd, it will send it to Anubis. Anubis will then "weigh
the soul" of the connection, and forward to httpd if deemed worthy.

We start by installing Anubis:

```
# pkg_add anubis
```

Anubis is configured using environment variables. On OpenBSD, Anubis
sources variables from `/etc/anubis.env`. A full list of environment
variables can be found on the [Setting up Anubis][anubis-setup] page
of the Anubis docs. For this setup, we configure Anubis to listen on
`127.0.0.1:8081` and have it forward requests to httpd at `127.0.0.1:8080`:

```sh
# /etc/anubis.env

# The address to listen on.
export BIND="127.0.0.1:8081"

# The address to forward approved HTTP traffic (httpd).
export TARGET="http://127.0.0.1:8080"

# The address to serve Prometheus-style metrics.
export METRICS_BIND="127.0.0.1:9090"
```

At this point, we've done enough to get Anubis up and running:

```
# rcctl enable anubis
# rcctl start anubis
```

Now that Anubis is running, we need to update [relayd(8)] to forward
requests to it instead of [httpd(8)]:

```diff
 # /etc/relayd.conf

 http protocol https {
     tls keypair jtm.cx
+    match header set "X-Real-IP" \
+        value "$REMOTE_ADDR"
     match header set "X-Forwarded-For" \
         value "$REMOTE_ADDR"
     match header set "X-Forwarded-By" \
         value "$SERVER_ADDR:$SERVER_PORT"
 }

 relay "https" {
     listen on egress port 443 tls
     protocol "https"
-    forward to 127.0.0.1 port 8080    # httpd
+    forward to 127.0.0.1 port 8081    # anubis
 }
```

Note that Anubis gets the client's IP from the `X-Real-IP` header.
It's not *strictly* necessary to add this header. If `X-Real-IP` isn't
provided, Anubis falls back to using `X-Forwarded-For`. However, using
`X-Real-IP` is good practice. Some more information can be found in the
[Client IP Headers][anubis-xff] section of the Anubis documentation.

Finally, we restart relayd to pick up the new configuration:

```
# rcctl restart relayd
```

And that's it!

## Custom Bot Policies

Anubis has one other configuration file aside from `/etc/anubis.env` called
the [bot policy file][anubis-policies]. This is a YAML document that spells
out what actions Anubis should take when evaluating requests. The [default
configuration](https://github.com/TecharoHQ/anubis/blob/main/data/botPolicies.yaml)
can be found in the Anubis source repository. This file is baked into
the executable, but Anubis can be configured to use a custom bot policy
file at runtime.

To use a custom policy, start by copying the default policy from upstream.
Unfortunately this file is updated fairly frequently (it's an arms race
after all), so the default bot policy differs from version to version,
and they're not necessarily compatible. When copying the default policy,
make sure that it matches the version of Anubis you have installed:

```
# pkg_info anubis | head -1
Information for inst:anubis-1.25.0v0
```

I'm running version 1.25.0. Once you know what version you're running,
the default policy for that specific version can be copied from upstream:

```
# mkdir /etc/anubis
# cd /etc/anubis
# ftp https://raw.githubusercontent.com/TecharoHQ/anubis/refs/tags/v1.25.0/data/botPolicies.yaml
```

Anubis can then be configured to use this local `botPolicies.yaml`
instead of the default one baked into the executable:

```diff
 # /etc/anubis.env

 # ...

+export POLICY_FNAME=/etc/anubis/botPolicies.yaml
```

## Some Additional Configuration

### Creating a Daemon User

By default, Anubis runs as the `www` user. I don't love this; it's common
practice to assign each daemon a dedicated user. It's easy to create a
dedicated `_anubis` user:

```
# useradd -g =uid -c "Anubis Daemon" -L daemon -s /sbin/nologin -d /var/empty _anubis
```

Now we can swap the user with a simple patch to `/etc/rc.d/anubis`:

```diff
 #!/bin/ksh

 daemon="sh -c '. /etc/anubis.env; /usr/local/sbin/anubis'"
-daemon_user="www"
+daemon_user="_anubis"
 daemon_logger=daemon.info

 . /etc/rc.d/rc.subr

 pexp="/usr/local/sbin/anubis${daemon_flags:+ ${daemon_flags}}"

 rc_bg=YES
 rc_reload=NO

 rc_cmd $1
```

### Redirecting Logs

The Anubis logs are noisy. The stock [rc.d(8)] script shipped with Anubis
is configured to forward logs to [syslogd(8)] at level `daemon.info`. I
don't like that `/var/log/daemon` is generally overrun with Anubis logs.
Adding the following block to the top of [syslog.conf(5)] will instruct
syslog to write logs to `/var/www/logs/anubis.log` instead.

```
# /etc/syslog.conf

# Redirect messages from anubis to its own log file.
!!anubis
*.*             /var/www/logs/anubis.log
!*

# ...
```

### Other Considerations

I'd love to add some additional OpenBSD-style security features to Anubis.
It should be straightforward to patch Anubis with support for [pledge(2)]
and [unveil(2)]. I also don't think it should be hard to run Anubis
in a chroot. This is left as an exercise to the reader.

[Anubis]: https://anubis.techaro.lol/
[anubis-xff]: https://anubis.techaro.lol/docs/admin/caveats-xff
[anubis-setup]: https://anubis.techaro.lol/docs/admin/installation
[anubis-policies]: https://anubis.techaro.lol/docs/admin/policies/

[httpd(8)]: http://man.openbsd.org/httpd.8
[httpd.conf(5)]: http://man.openbsd.org/httpd.conf.5
[rc.d(8)]: http://man.openbsd.org/rc.d.8
[relayd(8)]: http://man.openbsd.org/relayd.8
[syslog.conf(5)]: http://man.openbsd.org/syslog.conf.5
[syslogd(8)]: http://man.openbsd.org/syslogd.8
[pledge(2)]: http://man.openbsd.org/pledge.2
[unveil(2)]: http://man.openbsd.org/unveil.2
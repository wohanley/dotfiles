#!/bin/bash

mount --bind /home/wohanley/Videos /srv/nfs/videos
mount --bind /home/wohanley/Music/Library /srv/nfs/music

iptables -A INPUT -p tcp -m tcp --dport 2049 -j ACCEPT

systemctl start nfs-server

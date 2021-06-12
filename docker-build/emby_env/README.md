Centos with emby server and systemd.

docker run -d -p 8096:8096 -p 7359:7359/udp -p 1900:1900/udp --user $(id -u):$(id -g) -v /sys/fs/cgroup:/sys/fs/cgroup:ro -v $(host_path):/library bayonetc/emby_env

To access: http://container-host:8096

To use the Emby official image:

docker run -d --name embyserver -v /tank/software/config/emby:/config -v /tank:/mnt/share1 -v /home:/mnt/share2 --net=host -p 8096:8096 -p 8920:8920 emby/embyserver

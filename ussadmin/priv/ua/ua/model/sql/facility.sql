BEGIN;
--SQLite不强制VARCHAR的长度。你可以声明一个VARCHAR(10)，SQLite一样可以让你存放500个字符在里面。 并且它们会始终完整无缺——决不会被截断。
    CREATE TABLE `zone` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `alias` VARCHAR(75) UNIQUE NOT NULL,
        `nw` VARCHAR(100),
        `pwr` VARCHAR(100),
        `geo` VARCHAR(100),
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    );
    
    CREATE TABLE `device` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `ip` VARCHAR(15) UNIQUE NOT NULL,
        `hostname` VARCHAR(50) NOT NULL,
        `username` VARCHAR(50) NOT NULL,
        `password` VARCHAR(50) NOT NULL,
        `zone` INTEGER,
        `srvtype` INTEGER,
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(zone) REFERENCES zone(id)
    )
    ;
    
    CREATE TABLE `device_spec` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `os_release` VARCHAR(30),
        `os_bitwide` INTEGER,
        `cpu_num` INTEGER,
        `mem_total` INTEGER,
        `swap_total` INTEGER,
        `nw_speed` INTEGER,
        `drv` VARCHAR(30),
        `record_time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(id) REFERENCES device(id)
    )
    ;
    
    CREATE TABLE `device_stat` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `status` INTEGER,
        `uptime` VARCHAR(30),
        `cpu_load1` FLOAT,
        `cpu_load5` FLOAT,
        `cpu_load15` FLOAT,
        `mem_free` INTEGER,
        `mem_buffers` INTEGER,
        `mem_cached` INTEGER,
        `swap_free` INTEGER,
        `drv` VARCHAR(30),
        `record_time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(id) REFERENCES device(id)
    )
    ;
    
--    CREATE TABLE `srv_type` (
--        `id` INTEGER PRIMARY KEY NOT NULL,
--        `type` VARCHAR(10) NOT NULL
--    )
--    ;
--    INSERT INTO srv_type (id, type) VALUES (1, 'proxy'); 
--    INSERT INTO srv_type (id, type) VALUES (2, 'auth'); 
--    INSERT INTO srv_type (id, type) VALUES (3, 'account'); 
--    INSERT INTO srv_type (id, type) VALUES (4, 'container'); 
--    INSERT INTO srv_type (id, type) VALUES (5, 'object'); 
    
    --INSERT INTO device (id, ip, hostname, username, password) VALUES (0, '192.168.1.1', 'wss1', 'root', 'mdsmds');
    --INSERT INTO device_eval (id, cpu_num, mem_total, nw_speed, drv) VALUES (0, 16, 48, 1000, '{sda:160,sdb:320}');
    
--    CREATE TABLE `drive` (
--        `id` INTEGER PRIMARY KEY NOT NULL,
--        `device` INTEGER NOT NULL,
--        FOREIGN KEY(device) REFERENCES device(id)
--    );
    
--    CREATE TABLE `instance` (
--        `id` INTEGER PRIMARY KEY NOT NULL,
--        `device` INTEGER NOT NULL,
--        FOREIGN KEY(device) REFERENCES device(id)
--    );

    -- INSERT INTO user (username, password) VALUES('admin', 'admin');
     
COMMIT;

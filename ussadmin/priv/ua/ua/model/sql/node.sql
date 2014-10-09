BEGIN;
--SQLite不强制VARCHAR的长度。你可以声明一个VARCHAR(10)，SQLite一样可以让你存放500个字符在里面。 并且它们会始终完整无缺——决不会被截断。
--    CREATE TABLE `zone` (
--        `id` INTEGER PRIMARY KEY NOT NULL,
--        `name` VARCHAR(75) UNIQUE NOT NULL,
--        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
--     );
 
    CREATE TABLE `rack` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `name` VARCHAR(75) UNIQUE NOT NULL,
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    );
    
    CREATE TABLE `node` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `ip` VARCHAR(15) UNIQUE NOT NULL,
        `hostname` VARCHAR(50) NOT NULL,
        `user` VARCHAR(50) NOT NULL,
        `passwd` VARCHAR(50) NOT NULL,
        `rack` INTEGER REFERENCES `rack` (`id`) NOT NULL,
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )
    ;
    
    CREATE TABLE `node_dtls` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `cpu` VARCHAR(30), -- [(model,hz), (i5,2.6GHz), (i5,2.6GHz)]
        `mem` INTEGER, --16
        `swap` INTEGER, -- 16
        `nw` VARCHAR(30),
        `df` VARCHAR(30), -- [(fs, capacity, used, mounted_on), (/dev/sda1, 160, 80, /)]
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(id) REFERENCES node(id)
    )
    ;
    
    CREATE TABLE `node_srvc` (
        `id` INTEGER PRIMARY KEY NOT NULL,
        `mds` VARCHAR(30),
        `cds` VARCHAR(30), 
        `c60` VARCHAR(30),
        `ua` VARCHAR(30),  
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(id) REFERENCES node(id)
    )
    ;
    
--    CREATE TABLE `device_stat` (
--        `id` INTEGER PRIMARY KEY NOT NULL,
--        `status` INTEGER,
--        `uptime` VARCHAR(30),
--        `cpu_load1` FLOAT,
--        `cpu_load5` FLOAT,
--        `cpu_load15` FLOAT,
--        `mem_free` INTEGER,
--        `mem_buffers` INTEGER,
--        `mem_cached` INTEGER,
--        `swap_free` INTEGER,
--        `drv` VARCHAR(30),
--        `record_time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
--        FOREIGN KEY(id) REFERENCES device(id)
--    )
--    ;
    
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

BEGIN;

    CREATE TABLE `session` (
        `session_id` VARCHAR(128) UNIQUE NOT NULL,
        `atime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `data` TEXT
    );
     
COMMIT;
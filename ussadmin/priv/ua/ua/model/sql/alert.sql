BEGIN;

	CREATE TABLE `alert_info` (
	    `id` INTEGER PRIMARY KEY NOT NULL,
	    `level` VARCHAR(75) NOT NULL,
	    `info` VARCHAR NOT NULL,
	    `atime` TIMESTAMP DEFAULT (datetime('now', 'localtime')) NOT NULL,
			`etime` VARCHAR NOT NULL,
			`event` VARCHAR NOT NULL,
			`catagory` VARCHAR NOT NULL
	)
	;

COMMIT;

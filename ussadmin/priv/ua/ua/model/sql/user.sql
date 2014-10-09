BEGIN;
--	CREATE TABLE `permi` (
--	    `id` INTEGER PRIMARY KEY NOT NULL,
--	    `no` VARCHAR(75) UNIQUE NOT NULL,
--	    `permi` VARCHAR(200) NOT NULL
--	)
--	;
	
	CREATE TABLE `grop` (
	    `id` INTEGER PRIMARY KEY NOT NULL,
	    `name` VARCHAR(75) UNIQUE NOT NULL,
	    `grop` INTEGER REFERENCES `grop` (`id`),
	    `permis` VARCHAR(200) NOT NULL,
	    `atime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
	)
	;
	
	CREATE TABLE `user` (
		`id` INTEGER PRIMARY KEY NOT NULL,
		`name` VARCHAR(75) UNIQUE NOT NULL,
		`pwhash` VARCHAR(128) NOT NULL,
		`grop` INTEGER REFERENCES `grop` (`id`) NOT NULL,
		`atime` TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
		-- FOREIGN KEY(privilege) REFERENCES privilege(id)
	)
	;

	INSERT INTO grop (name, grop, permis) VALUES('root', -1, '000');
	INSERT INTO user (name, pwhash, grop) VALUES('root', '63a9f0ea7bb98050796b649e85481845', 1);
	
	
    CREATE VIEW jstree_view AS SELECT name AS nodeName, id AS NodeId, grop AS fnodeId , 'group' AS nodeType FROM grop UNION 
    SELECT name AS nodeName, id AS nodeId, grop AS fnodeId, 'user' AS nodeType FROM user;
    
COMMIT;

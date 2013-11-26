---
title: Postgres 的文件存储位置
published: 2013-11-21
excerpt: 
tags: DB, Postgres, Algorithm
---


initdb 的时候会指定一个 PGDATA 目录，这就是 PostgresQL 存储数据的地方。典型的位置是在 /var/lib/postgres/data 或 /home/postgres/data 。PGDATA 下面各项存储的内容大概是：

文件或目录名    存储内容
-------         ----------
PG_VERSION         PostgresQL 实例的版本号如 9.3 之类的
base             每个 database 会在 base 目录下有一个子目录
global             Postgres 自己的 meta 数据库存放的地方（全局 DB）
pg_xlog         WAL(Write Ahead Log 预写式日志）存放的地方
其他            其他不知道干啥的目录还有好多

base 目录是最重要的一个目录，放的是每一个 database 的数据。base 目录里的每一个数字目录对于一个 database 的 oid， 可以通过 查看 pg_database 这张表查看每一个 数据库的 oid 。

    lai=# select oid, datname from pg_database ;
      oid  |  datname  
    -------+-----------
         1 | template1
     12031 | template0
     12036 | postgres
     16385 | lai
    (4 rows)

每一张表的数据（大部分）又是放在 base/(dboid)/(relfilenode) 这个文件里面：

    lai=# select relname, relowner, relfilenode from pg_class where relowner = 16384;
            relname        | relowner | relfilenode 
    -----------------------+----------+-------------
     pg_toast_24589        |    16384 |       24592
     pg_toast_24589_index  |    16384 |       24594
     pg_toast_24595        |    16384 |       24598
     pg_toast_24595_index  |    16384 |       24600
     item_id_seq           |    16384 |       24601
     Feed_pkey             |    16384 |      167963
     feed                  |    16384 |       24589
     item                  |    16384 |       24595
     pg_toast_168003       |    16384 |      168006
     pg_toast_168003_index |    16384 |      168008
     tmp                   |    16384 |      168003
    (11 rows)

feed 这张表数据在 base/16386/24589 文件里，item 这张表的数据放在 base/16386/24595 这个文件里。也可以用 pg_relation_filepath 这个函数查询：

    lai=# select pg_relation_filepath('item');
     pg_relation_filepath 
    ----------------------
     base/16385/24595
    (1 row)

当然实际的存储不会这么简单。每一张表的文件都会有一些附加的存储文件，如文件名后加上 _fsm 的是空闲空间映射表 (Free Space Map)。另外 base/(dboid)/(relfilenode) 这个文件超过 1GB 以后，Postgres 会把这个文件拆分成不超过 1G 的多个文件，文件末尾加上 .1 .2 .3 ... 做编号。 如 24589 24589.1 24589.2 。据说这是因为某些文件系统支持的最大文件大小有限制(如 fat32 只支持最大 4G )的文件。



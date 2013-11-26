---
title: Postgres 中的简单压缩算法
published: 2013-11-27
excerpt: 
tags: DB, Postgres, Algorithm
---

Postgres 的页面(page)大小是固定的 8k，同一行的数据必须在同也个页面内，但是 Postgres 需要支持变长的数据类型(如 varchar)，是可能超过 8k 的。解决方案是所谓的 [TOAST](http://www.postgresql.org/docs/current/static/storage-toast.html) (The Oversized-Attribute Storage Technique, 过长字段存储技术)。

TOAST 解决的思路一个是压缩，一个是页外存储。两个可以结合：页外压缩存储。页外存储就是在每个有变长字段表的 table 存储文件外再创建一个 .toast 结尾文件，过长字段存放在 .toast 文件，并将 offset 放在原 table 文件中替代。

Postgres 的压缩采用的是一个极简单的 lz 字典压缩算法。从解压过程来理解其原理的话非常简单：

    sp= 11110000 | 0x41 | 0x41 | 0x42 | 0x43 | 0x01 | 0x00 | 0x05 | 0x00 | 0x09 | 0x00 | 0x0f | 0x00  | 0x0e  |
        ———————————————————————————————————————————————————————————————————————————————————————————————————————
        control  | data | data | data | data | len4 + off12| len4 + off12| len4 + off12| len4 + off12 | len8  |
        ———————————————————————————————————————————————————————————————————————————————————————————————————————
                 | 'A'  | 'B'  | 'C'  | 'D'  | len:4 off:0 | len:8 off:0 | len:16 off:0| len:18 off:0 | len:14|
        ____________|      |      |      |         |             |             |              |              |
        |__________________|      |      |         |             |             |              |       +      |
        ||________________________|      |         |             |             |              |———————————————
        |||______________________________|         |             |             |              |
        ||||  _____________________________________|             |             |              |
        ||||  |     _____________________________________________|             |              |
        ||||  |     |          ________________________________________________|              |
        ||||  |     |          |                       _______________________________________|
        ||||  |     |          |                       |
        ||||{4-}{---8--}{------16------}{--------------32--------------}
    dp= ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD


sp = [oxf0,0x41,0x41,0x41,0x41,0x01,0x00,0x05,0x00,0x09,0x00,0x0d,0x00,0x0e] 这样的一个压缩串能解压成什么呢？首先，先读一个 control byte，在这个例子里面就是 0xf0，也就是二进制的 11110000。从最低位开始看：

* 每看到一个 0 bit，就将 sp 的下一个字节原样写到结果 dp 里。
* 每看到一个 1 bit，就再读两个字节 byte1 和 byte2, len = [byte1 的低4位] + 3，off = [byte1 的高4位] * 256 + [byte2]。如果 len == 18, 那么再读一个字节 byte3，len += byte3。然后从 dp 的末尾往前 off 个字节拷贝 len 个字节到 dp 里。

这样我们一个长度为14字节的 sp 解压成一个 长度为64字节的dp，压缩比为14/64 ~= 22%。代码在 postgres/src/backend/utils/adt/pg_lzcompress.c 。简单利落得令人发指。

~~~ {lang="c"}
void pglz_decompress(const PGLZ_Header *source, char *dest) {
	const unsigned char *sp;
	const unsigned char *srcend;
	unsigned char *dp;
	unsigned char *destend;
	sp = ((const unsigned char *) source) + sizeof(PGLZ_Header);
	srcend = ((const unsigned char *) source) + VARSIZE(source);
	dp = (unsigned char *) dest;
	destend = dp + source->rawsize;
	while (sp < srcend && dp < destend) {
		unsigned char ctrl = *sp++;
		int			ctrlc;
		for (ctrlc = 0; ctrlc < 8 && sp < srcend; ctrlc++) {
			if (ctrl & 1)
			{
				int32		len;
				int32		off;

				len = (sp[0] & 0x0f) + 3;
				off = ((sp[0] & 0xf0) << 4) | sp[1];
				sp += 2;
				if (len == 18)
					len += *sp++;
				if (dp + len > destend) {
					dp += len;
					break;
				}
				while (len--) {
					*dp = dp[-off];
					dp++;
				}
			}
			else {
				if (dp >= destend)		/* check for buffer overrun */
					break;				/* do not clobber memory */

				*dp++ = *sp++;
			}
			ctrl >>= 1; 				/* Advance the control bit */
		}
	}
}

~~~


再来看压缩过程，压缩比解压复杂，就好比给车打气比放气难，又好比吃饭比做饭容易，又好比考公务员比下海难。代码的实现比较复杂，但是原理还是比较直观的。

给定一个待压缩串 source，压缩的结果输出到 dest。数据还是用上面的例子，只是过程反过来。

 * 如果 control byte 没有分配或已经用完的话，在 dest 里分配一个 control byte
 * 取 source 串的下一个待压缩序列长度至少为 3 的尽量长的序列
 * 如果在已压缩的 source 串中能找到一个连续的串和待压缩串相同，那么 control bit 置 1，并将 source 串中的 offset 和 len 写入 dest。
 * 如果没有找到，那么将 control bit 置 0，将 source 中的下一个字节直接写入 dest。

如此重复，直到压缩完成 source 串中的所有字节为止。具体代码可以参看 pglz_decompress.c 文件。


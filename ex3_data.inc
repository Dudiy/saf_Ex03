; file: ex2.asm
; Dudi Yecheskel , ID 200441749

.data
	numrows	BYTE 4
	numcols BYTE 4
	board	BYTE 3, 23, 14, 'S',
				 19, 'S', 'E', 12,
				 9, 'S', 33, 17,
				 1, 10, 'E', 5	
	moves BYTE 5, 5, 4, 2, ';'
	nomoves BYTE 5

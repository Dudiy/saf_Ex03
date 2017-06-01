; file: ex2.asm
; Dudi Yecheskel , ID 200441749

.data
	numrows	BYTE 4
	numcols BYTE 4
	board	BYTE 3, 2, 14, 2,
				 19, 'S', 'E', 'E',
				 9, 'S', 31, 17,
				 1, 10, 3, 5	
	moves BYTE 5, 5, 4, 2, ';'
	nomoves BYTE 5

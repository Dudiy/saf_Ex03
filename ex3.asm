; file: ex3.asm
; Dudi Yecheskel , ID 200441749

TITLE ex3
; This program simulates a board game as described in the instructions

INCLUDE Irvine32.inc
INCLUDE ex3_data.inc

.data
	myName BYTE "Dudi Yecheskel  ID:200441749  EX2",10,0
	gameFinStr BYTE "gameFin: " , 0
	gamefin BYTE 0									;init to 0 - will change to 1 only if the player wins
	scoreStr BYTE "score: " , 0
	score DWORD 0									;sum of cell values if player wins or error identifier if player looses
	moveNumStr BYTE "moveNum: ", 0
	moveNum WORD 0									;the number of moves made before the game ended
	rearrangedBoard BYTE LENGTHOF board DUP(?)		;a copy of the given board, rearanged by stepping order
	currRow BYTE 0									;the current row being copied
	lastCellOnBoard DWORD ?							;the address of the last cell on the board

.code
	myMain PROC
	;Print my name
	mov edx, OFFSET myName
	call writestring

	;======================  sort and copy the given board in a to a new place in memory (rearangedBoad) ======================
	mov esi, OFFSET board
	mov edi, OFFSET rearrangedBoard

	;set esi to the bottom left cell
	add esi, SIZEOF board	
	movsx ebx, BYTE PTR numCols			;if we want this to be generic we can multiply this value by 'TYPE board' (here its 1 so there is no need)
	sub esi, ebx

;main loop - each itteration copies a single row from board to rearrangedBoard
nextRow:
	;compare curr to board, if curr < board then finished copying
	; (*) it doesn't matter if the last row is even or odd, in both cases the first cell is not less than 'OFFSET board'
	cmp esi, OFFSET board
	jl endOfCopy

	;check parity 
	movsx eax, currRow
	and eax, 1
	cmp eax, 1			;(eax = 1 => odd row number)
	je copyOddRow
	jne copyEvenRow

copyOddRow:
	;jump to the end of the row to copy backwards
	movsx eax, BYTE PTR numCols
	add esi, eax
	dec esi

	movsx ecx, BYTE PTR numCols
	; cells are copied one by one because we don't know in advance the value of numCols
	copyCell1:
		mov al, BYTE PTR [esi]
		mov [edi], al
		inc edi
		dec esi
		loop copyCell1

	;set esi to the beginning of the row above
	movsx eax, BYTE PTR numCols
	sub esi, eax
	inc esi
	inc currRow
	jmp nextRow

copyEvenRow:
	movsx ecx, BYTE PTR numCols
	copyCell2:
		mov al, BYTE PTR [esi]
		mov [edi], al
		inc edi
		inc esi
		loop copyCell2

	;set esi to the beginning of the row above
	movsx eax, BYTE PTR numCols
	sub esi, eax
	sub esi, eax
	inc currRow
	jmp nextRow

endOfCopy:
	;set lastCellOnBoard to be the address of the last cell
	mov eax, OFFSET rearrangedBoard
	add eax, SIZEOF rearrangedBoard
	sub eax, TYPE rearrangedBoard
	mov lastCellOnBoard, eax
	;============================================ copying of board finished ============================================

	;============================================ start making moves on the board ============================================
	;initialize the game
	mov esi, OFFSET rearrangedBoard

makeMove:
	movsx eax, moveNum						;current move index - initialized to 0
	movsx ebx, BYTE PTR moves[eax] 			;get the current move value

	cmp ebx, ';'							;if all moves done (reached ';') end game
	je endOfMoves
	
	add esi, ebx							;move esi position according to move value
	inc moveNum								;increment move counter
	cmp esi, lastCellOnBoard				;if passed the last cell on the board = error
	jg passedLastCellOnBoard

checkCurrCell:
	cmp BYTE PTR [esi], 'E'
	je E							;jump if the cell is 'E'
	cmp BYTE PTR [esi], 'S'
	je S							;jump if the cell is 'S'
	
	;if the current cell holds a digit add its value to score
	digit:
		movsx ecx, BYTE PTR [esi]	;add the value in current cell to score
		add score, ecx
		jmp makeMove				;else make another move

	;if the current cell is 'E' then move one row up and check the value of the cell again
	;the number of moves to make in order to go up one row: ((numCols -(currCell % numCols)) * 2) + 1
	E:
		;calculate: numCols - (currCell % numCols)
		mov edx, 0
		mov eax, esi
		sub eax, OFFSET rearrangedBoard
		movsx ebx, numCols
		div ebx
		movsx eax, numCols
		dec eax
		sub eax, edx		;eax = numCols - (currCell % numCols)

		;calculate ((numCols -(esi % numCols)) * 2) + 1
		mov ebx, 2
		mul ebx				;eax = ((numCols -(currCell % numCols)) * 2)
		inc eax				;eax = ((numCols -(currCell % numCols)) * 2) + 1
		
		add esi, eax		;move esi to row above on the board

		;check if after "climbing" up we are above the board
		cmp esi, lastCellOnBoard
		jg foundEOnLastRow
		jmp checkCurrCell

	;if the current cell is 'S' then down one row and check the value of the cell again
	;the number of moves to make in order to go down one row: - ((currCell % numCols) * 2) + 1
	S:
		;calculate (currCell % numCols)
		mov edx, 0
		mov eax, esi
		sub eax, OFFSET rearrangedBoard
		movsx ebx, numCols
		div ebx
		mov eax, edx	;eax = (currCell % numCols)

		;calculate ((esi % numCols) * 2) + 1
		mov ebx, 2
		mul ebx			;eax = ((currCell % numCols) * 2)
		inc eax			;eax = ((currCell % numCols) * 2) + 1

		sub esi, eax	;move esi to row below on the board
		
		;check if after "sliding" down we are under the board
		cmp esi, OFFSET rearrangedBoard
		jl foundSOnFirstRow
		jmp checkCurrCell

	;player has played all moves
	endOfMoves:
		cmp lastCellOnBoard, esi
		jne outOfMoves		
		mov gamefin, 1			;last cell reached on last move => player won
		jmp gameEnded

	;============================================ errors ============================================

	;stepped on 'S' while on the first (bottom) row
	foundSOnFirstRow:
		mov score, 1
		jmp gameEnded

	;stepped on 'E' while on the last (top) row
	foundEOnLastRow:
		mov score, 2
		jmp gameEnded

	;the player has passed the last cell on the board
	passedLastCellOnBoard:
		mov score, 3
		jmp gameEnded

	;the player has run out of moves before reaching the last cell on the board
	outOfMoves:
		mov score, 4
		jmp gameEnded

	;============================================ end of game - print results ============================================
	gameEnded:
		;print gameFin
		mov edx, OFFSET gameFinStr
		call writeString
		movsx eax, gamefin
		call writedec
		call CRLF
		
		;print score
		mov edx, OFFSET scoreStr
		call writeString
		mov eax, score
		call writedec
		call CRLF

		;print moveNum
		mov edx, OFFSET moveNumStr
		call writeString
		movsx eax, moveNum
		call writedec
		call CRLF

	exit
myMain ENDP

END myMain
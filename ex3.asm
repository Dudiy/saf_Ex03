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
	;moveNum WORD 0									;the number of moves made before the game ended
	rearrangedBoard BYTE LENGTHOF board DUP(?)		;a copy of the given board, rearanged by stepping order
	currRow BYTE 0									;the current row being copied
	lastCellOnBoard DWORD ?							;the address of the last cell on the board
	;moveseries SBYTE 5, 5, 1, ';'	; TODO: Important change to nomoves somehow
	moveseries SBYTE 10 DUP(?)	; TODO: Important change to nomoves somehow
	MIN_DIGIT = 1
	MAX_DIGIT = 40
.code

myMain PROC
	;Print my name
	mov edx, OFFSET myName
	call writestring

	push OFFSET board
	movzx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call checkboard
	cmp eax, -1
	je invalidBoard
	call copyAndRearangeBoard	
	

	; findshortseries(moveSeriesPtr, nomoves, rearrangedBoardPtr, numRows, numCols)
	push OFFSET moveseries
	movsx eax, nomoves
	push eax
	push OFFSET rearrangedBoard
	movsx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call findshortseries

	push OFFSET moveseries
	push OFFSET rearrangedBoard
	movsx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call writescore

	invalidBoard:
		mov score, -1
		mov moveseries, -1

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
		movsx eax, byte PTR [ebp + moveNum]
		call writedec
		call CRLF

		exit
myMain ENDP

copyAndRearangeBoard PROC	
	PUSHAD	
	mov esi, OFFSET board
	mov edi, OFFSET rearrangedBoard

	;set esi to the bottom left cell
	;add esi, SIZEOF board	
	;movsx ebx, BYTE PTR numCols			;if we want this to be generic we can multiply this value by 'TYPE board' (here its 1 so there is no need)
	;sub esi, ebx

	push OFFSET board
	movzx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call moveEsiToBottomLeft

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
	
	POPAD
	ret
copyAndRearangeBoard ENDP

; original board is transfered, board is checked from last row to the first
checkboard PROC
	i_NumCols = 8
	i_NumRows = i_NumCols + 4
	i_BoardPtr = i_NumRows + 4
	
	push ebp
	mov ebp, esp
	push esi
	push ebx
	push ecx
	push edx

	;set eax to 1 , will be changed to -1 if the board is invalid
	mov eax, 0
	mov esi, [ebp + i_BoardPtr]
	mov ecx, [ebp + i_NumRows]
	checkRows:
		mov ebx, ecx
		mov ecx, [ebp + i_NumCols]
		checkCols:
			cmp [esi], byte PTR 'S'
			;if 'S' check cell under
			jne notS							
			mov edx, [ebp + i_NumCols]
			;if equal then there is E under S
			cmp [esi + edx], byte PTR 'E'		
			je invalidBoard
			jmp nextCol

			notS:
			;if 'E' there can't be S above (we are checking from top to bottom)
			cmp [esi], byte PTR 'E'				
			je nextCol

			;not 'S' or 'E' so must be a digit between 1 and 40
			cmp [esi], byte PTR MIN_DIGIT
			jl invalidBoard

			cmp [esi], byte PTR MAX_DIGIT
			jg invalidBoard

			nextCol:
				inc esi
				LOOP checkCols
		mov ecx, ebx
		LOOP checkRows

	;check if there is an 'E' on the top row
	mov esi, [ebp + i_BoardPtr]
	mov ecx, [ebp + i_NumCols]
	checkLastRowForE:
		cmp [esi], byte PTR 'E'
		je invalidBoard
		inc	esi
		LOOP checkLastRowForE

	;check if there us an 'S' on the bottom row
	;get to bottom left cell	
	push [ebp + i_BoardPtr]
	push [ebp + i_NumRows]
	push [ebp + i_NumCols]
	call moveEsiToBottomLeft

	checkFirstRowForS:
		;loop trough cells of first row
		mov ecx, [ebp + i_NumCols]
		lp2:
			cmp [esi], byte PTR 'S'
			je invalidBoard
			inc esi
			LOOP lp2

	; if passed all checks, jump to end of procedure
	jmp endOfProc
	
	; if at any point an invalid cell value is found, will jump to here
	invalidBoard:
		mov eax, -1

	endOfProc:
		pop edx
		pop ecx
		pop ebx
		pop esi
		mov esp, ebp
		pop ebp
		ret 12
checkboard ENDP

; set ESI to point at the bottom left cell of the given board
; parameters (boardPtr, numRows, numCols)
moveEsiToBottomLeft PROC
	i_NumCols = 8
	i_NumRows = i_NumCols + 4
	i_BoardPtr = i_NumRows + 4

	push ebp
	mov ebp, esp
	push ecx

	mov esi, [ebp + i_BoardPtr]
	mov ecx, [ebp + i_NumRows]
	dec ecx
	nextRow:
		add esi, [ebp + i_NumCols]
		LOOP nextRow
		
	pop ecx
	mov esp, ebp
	pop ebp
	ret 12
moveEsiToBottomLeft ENDP

; gets a pointer to a series of moves, and returns the next series
; parameters (movesSeriesPtr, lengthOfSeries)
; assumption: all values of the series are numbers between 1-6
nextmove PROC
	i_lengthOfSeries = 8
	i_MovesSeriesPtr = i_lengthOfSeries + 4

	push ebp
	mov ebp, esp
	push esi
	push ecx
	push ebx

	; init EAX to 0, will be changed to 1 if series is 66...6
	mov eax, 0
	mov esi, [ebp + i_MovesSeriesPtr]
	mov ebx, [ebp + i_lengthOfSeries]
	add esi, ebx
	incPrev:
		dec esi
		cmp esi, [ebp + i_MovesSeriesPtr]
		jl all6Series
		cmp [esi], byte PTR 6
		jl isLessThan6
		mov [esi], byte PTR 1
		LOOP incPrev
	
	isLessThan6:
		add [esi], byte PTR 1
		jmp endProc

	all6Series:
		mov eax, 1
		jmp endProc

	endProc:
	pop ebx
	pop ecx
	pop esi
	mov esp, ebp
	pop ebp
	ret 8
nextmove ENDP

; check if a given move series solves the board
; parameters (moveSeriesPtr, rearrangedBoardPtr, numRows, numCols)
; assumption: the board is valid and rearanged
checksolved PROC
	i_NumCols = 8
	i_NumRows =  i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_MovesSeriesPtr = i_RearrangedBoardPtr + 4

	moveNum = - 4
	currentScore = moveNum - 4

	push ebp
	mov ebp, esp
	; init moveNum and score to 0
	push 0
	push 0
	push esi
	push ebx
	push ecx

	;============================================ start making moves on the board ============================================
	;initialize the game
	mov esi, [ebp + i_RearrangedBoardPtr]

	makeMove:
		mov ebx, [ebp + i_MovesSeriesPtr]
		add ebx, [ebp + moveNum]				;current move index - initialized to 0

		movsx ebx, byte PTR [ebx] 						;get the current move value

		cmp ebx, ';'							;if all moves done (reached ';') end game
		je endOfMoves
	
		add esi, ebx							;move esi position according to move value
		inc byte Ptr [ebp + moveNum]			;increment move counter
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
			add [ebp + currentScore], ecx
			jmp makeMove				;else make another move

		;if the current cell is 'E' then move one row up and check the value of the cell again
		E:
			push [ebp + i_RearrangedBoardPtr]
			push [ebp + i_NumCols]
			call stepOnE
			jmp checkCurrCell

		;if the current cell is 'S' then down one row and check the value of the cell again
		S:
			push [ebp + i_RearrangedBoardPtr]
			push [ebp + i_NumCols]
			call stepOnS
			jmp checkCurrCell

		;player has played all moves
		endOfMoves:
			cmp lastCellOnBoard, esi
			jne outOfMoves		
			mov eax, [ebp + currentScore]			;last cell reached on last move => player won
			jmp endOfProc

		;============================================ errors ============================================

		;the player has passed the last cell on the board
		passedLastCellOnBoard:
			mov eax, -1
			jmp endOfProc

		;the player has run out of moves before reaching the last cell on the board
		outOfMoves:
			mov eax, -1
			jmp endOfProc

	endOfProc:
		pop ecx
		pop ebx
		pop esi
		mov esp, ebp
		pop ebp
		ret 16		
checksolved ENDP

; find the shortest series that solves the board and write it in moveSeries
; parameters (moveSeriesPtr, nomoves, rearrangedBoardPtr, numRows, numCols)
; assumption: the board is valid and rearanged
findshortseries PROC
	i_NumCols = 8
	i_NumRows = i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_Nomoves = i_RearrangedBoardPtr + 4
	i_MovesSeriesPtr = i_Nomoves + 4

	lengthOfSeries = -4

	push ebp
	mov ebp, esp
	; init lengthOfSeries
	push 1
	push ebx

	; init moveSeries 
	mov ebx, [ebp + i_MovesSeriesPtr]
	mov [ebx], byte PTR 1
	inc ebx
	mov [ebx], byte PTR ';'

	loop1:
		push [ebp + i_MovesSeriesPtr]
		push [ebp + i_RearrangedBoardPtr]
		push [ebp + i_NumRows]
		push [ebp + i_NumCols]
		call checksolved
		cmp eax, -1
		jne solved

		push [ebp + i_MovesSeriesPtr]
		push [ebp + lengthOfSeries]
		call nextMove
		cmp eax, 1
		je incSeriesLength
		LOOP loop1

	incSeriesLength:
		mov	ebx, [ebp + i_Nomoves]
		cmp ebx, [ebp + lengthOfSeries]
		je noPossibleSolution

		mov ebx, [ebp + i_MovesSeriesPtr]
		add ebx, [ebp + lengthOfSeries]
		mov [ebx], byte PTR 1
		inc ebx
		mov [ebx], byte PTR ';'

		inc byte PTR [ebp + lengthOfSeries]
		LOOP loop1

	solved:		
		jmp endOfProc
	
	noPossibleSolution:
		mov [ebp + i_MovesSeriesPtr], sbyte PTR -1
		mov eax, -1

	endOfProc:
		pop ebx
		mov esp, ebp
		pop ebp
		ret 20
findshortseries ENDP

; increment ESI to the row above
; parameters (rearrangedBoardPtr, numCols)
; assumptions: rearrangedBoardPtr points to a valid board
stepOnE PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumCols + 4
	push ebp
	mov ebp, esp
	push eax
	push ebx
	push edx

	;calculate numCols - (currCell % numCols)
	push [ebp + i_RearrangedBoardPtr]
	push [ebp + i_NumCols]
	call calc_ESI_Mod_NumCols				; eax = (currCell % numCols)
	mov ebx, eax
	mov eax, [ebp + i_NumCols]
	dec eax
	sub eax, ebx							;eax = numCols - (currCell % numCols)

	;calculate ((numCols -(esi % numCols)) * 2) + 1
	mov ebx, 2
	mul ebx				;eax = ((numCols -(currCell % numCols)) * 2)
	inc eax				;eax = ((numCols -(currCell % numCols)) * 2) + 1
		
	add esi, eax		;move esi to row above on the board

	pop edx
	pop ebx
	pop eax
	mov esp, ebp
	pop ebp
	ret 8
stepOnE ENDP

; decrement ESI to the row above
; parameters (rearrangedBoardPtr, numCols)
; assumptions: rearrangedBoardPtr points to a valid board
stepOnS PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumRows + 4
	push ebp
	mov ebp, esp
	push eax
	push ebx
	push edx

	; calculate (esi % numCols) => eax
	push [ebp + i_RearrangedBoardPtr]
	push [ebp + i_NumCols]
	call calc_ESI_Mod_NumCols
	
	;calculate ((esi % numCols) * 2) + 1
	mov ebx, 2
	mul ebx			;eax = ((currCell % numCols) * 2)
	inc eax			;eax = ((currCell % numCols) * 2) + 1
	
	sub esi, eax	;move esi to row below on the board

	pop edx
	pop ebx
	pop eax
	mov esp,ebp
	pop ebp
	ret 8 
stepOnS ENDP

; calculates esi (currentCell) modulos numCols  => currCell%numCols, returns the value in eax register
; parameters (rearrangedBoardPtr, numCols)
calc_ESI_Mod_NumCols PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumRows + 4
	push ebp
	mov ebp, esp
	push ebx
	push edx

	mov edx, 0
	mov eax, esi
	sub eax, [ebp + i_RearrangedBoardPtr]
	mov ebx, [ebp + i_NumCols]
	div ebx
	mov eax, edx				;eax = (currCell % numCols)

	pop edx
	pop ebx
	mov esp, ebp
	pop ebp
	ret 4
calc_ESI_Mod_NumCols ENDP


; given a series of moves, calculate the score
; parameters (moveSeriesPtr, rearrangedBoardPtr, numRows, numCols)
; assumption: the board is valid and rearanged, the set of moves is valid
writescore PROC
	i_NumCols = 8
	i_NumRows =  i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_MovesSeriesPtr = i_RearrangedBoardPtr + 4

	push ebp
	mov ebp, esp
	push esi
	push ebx
	push ecx

	;============================================ start making moves on the board ============================================
	;initialize the game
	mov eax, 0			;init the score to 0
	mov esi, [ebp + i_RearrangedBoardPtr]

	makeMove:
		mov ebx, [ebp + i_MovesSeriesPtr]
		movsx ebx, byte PTR [ebx] 				;get the current move value
		cmp ebx, ';'							;if all moves done (reached ';') end game
		je endOfMoves

		add esi, ebx							;move esi position according to move value

	checkCurrCell:
		cmp BYTE PTR [esi], 'E'
		je E							;jump if the cell is 'E'
		cmp BYTE PTR [esi], 'S'
		je S							;jump if the cell is 'S'
	
		;if the current cell holds a digit add its value to score
		digit:
			movsx ebx, BYTE PTR [esi]
			add eax, ebx								;add the value in current cell to score
			inc DWORD PTR [ebp + i_MovesSeriesPtr]		;move the movesSeries pointer to the next move
			jmp makeMove								;else make another move

		;if the current cell is 'E' then move one row up and check the value of the cell again
		E:
			push [ebp + i_RearrangedBoardPtr]
			push [ebp + i_NumCols]
			call stepOnE
			jmp checkCurrCell

		;if the current cell is 'S' then down one row and check the value of the cell again
		S:
			push [ebp + i_RearrangedBoardPtr]
			push [ebp + i_NumCols]
			call stepOnS
			jmp checkCurrCell

	;player has played all moves
	endOfMoves:
		pop ecx
		pop ebx
		pop esi
		mov esp, ebp
		pop ebp
		ret 16		
writescore ENDP

END myMain

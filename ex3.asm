; file: ex3.asm
; Dudi Yecheskel , ID 200441749

TITLE ex3
; This program simulates a board game as described in the instructions
; the program finds the shortest valid move series possible that gets a player
; to the last cell in the board. The length of the series will be at most 
; nomoves (a parameter given in the "ex3_data.inc" file).
; if the board is invalid, or no series of moves is found with less moves than nomoves
; then the value of score and the first move in moveseries will be -1.

INCLUDE Irvine32.inc
INCLUDE ex3_data.inc

.data
	myName BYTE "Dudi Yecheskel  ID:200441749  EX2", 10, 0
	moveseries SBYTE 127*127 DUP(?)					; the resulting series of moves (board size is at most 127*127)
	rearrangedBoard BYTE LENGTHOF board DUP(?)		; a copy of the given board, rearanged by stepping order
	lastCellOnBoard DWORD ?							; the address of the last cell on the board
	score DWORD 0									; sum of cell values if player wins or error identifier if player looses
	MIN_DIGIT = 1									; the minimal digit value of a cell in a valid board
	MAX_DIGIT = 40									; the maximal digit value of a cell in a valid board

.code
myMain PROC
	;Print my name
	mov edx, OFFSET myName
	call writestring

	; checkBoard(boardPtr, numCols, numRows)
	push OFFSET board
	movzx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call checkboard
	cmp eax, -1
	je invalidBoard

	; copyAndRearangeBoard(boardPtr, rearangedBoardPtr, numRows, numCols, lastCellOnBoardPtr)
	push OFFSET board
	push OFFSET rearrangedBoard
	movzx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	push OFFSET lastCellOnBoard
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
	cmp eax, -1
	je invalidBoard

	; writeScore( moveSeriesPtr, rearangedBoardPtr, numRows, numCols)
	push OFFSET moveseries
	push OFFSET rearrangedBoard
	movsx eax, numrows
	push eax
	movzx eax, numcols
	push eax
	call writescore
	mov score, eax
	jmp printRes

	invalidBoard:
		mov score, -1
		mov moveseries, -1
	
	; printResults(moveseriesPtr, score)
	printRes:
		push OFFSET moveseries
		mov eax, [score]
		push eax
		call printResults

	exit
myMain ENDP

; ============================================================= Procedures required in exercise =============================================================

; original board is transfered, board is checked from last row to the first
; after this procedure if the board is valid eax = 0 else eax = 1
; parameters: boardPtr, numRows, numCols
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

	;set eax to 0, will be changed to -1 if the board is invalid
	mov eax, 0

	;check if there is an 'E' on the top row
	mov esi, [ebp + i_BoardPtr]
	mov ecx, [ebp + i_NumCols]
	checkLastRowForE:
		cmp [esi], byte PTR 'E'
		je invalidBoard
		inc	esi
		LOOP checkLastRowForE
	
	;check if there is an 'S' on the bottom row
	;get to bottom left cell using procedure: moveEsiToBottomLeft(boardPtr, numRows, numCols)
	push [ebp + i_BoardPtr]
	push [ebp + i_NumRows]
	push [ebp + i_NumCols]
	call moveEsiToBottomLeft

	;loop trough cells of first row
	mov ecx, [ebp + i_NumCols]
	checkFirstRowForS:
		cmp [esi], byte PTR 'S'
		je invalidBoard
		inc esi
		LOOP checkFirstRowForS

	; loop to check all rows of the board
	mov esi, [ebp + i_BoardPtr]	
	mov ecx, [ebp + i_NumRows]
	checkRows:
		mov ebx, ecx
		; loop to check all columns in the the current row
		mov ecx, [ebp + i_NumCols]
		checkCols:
			cmp [esi], byte PTR 'S'
			jne notS							; if 'S' check cell under				
			mov edx, [ebp + i_NumCols]			; edx = distance to cell under current cell
			cmp [esi + edx], byte PTR 'E'		; compare cell under with 'E'
			je invalidBoard						; if equal then there is E under S
			jmp nextCol

			notS:
			;if 'E' there can't be S above so there is no need to check (we are checking from top to bottom)
			cmp [esi], byte PTR 'E'				
			je nextCol

			;not 'S' or 'E' validate that the cell value is a digit between 1 and 40
			cmp [esi], byte PTR MIN_DIGIT
			jl invalidBoard
			cmp [esi], byte PTR MAX_DIGIT
			jg invalidBoard

			nextCol:
				inc esi
				LOOP checkCols

		mov ecx, ebx
		LOOP checkRows

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

; gets a pointer to a series of moves, and returns the next series
; after this procedure if the series is 66...6 eax = 1, 
; else eax = 0 and the series will be incremented.
; assumption: all values of the series are numbers between 1-6
; parameters: moveseriesPtr, lengthOfSeries
nextmove PROC
	i_LengthOfSeries = 8
	i_MoveseriesPtr = i_LengthOfSeries + 4

	push ebp
	mov ebp, esp
	push esi
	push ecx

	; initialize EAX to 0, will be changed to 1 if series is 66...6
	; [esi] = currentMovesereisValue
	mov eax, 0
	mov esi, [ebp + i_MoveseriesPtr]
	add esi, [ebp + i_LengthOfSeries]
	incPrev:
		dec esi
		cmp esi, [ebp + i_MoveseriesPtr]		; check is esi is pointing to a place in memory before moveSeriesPtr
		jl all6Series							; if esi < moveseriesPtr, the current series is all 6
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
	pop ecx
	pop esi
	mov esp, ebp
	pop ebp
	ret 8
nextmove ENDP

; check if a given move series solves the board
; after this procedure if the series is given solves the board
; the value of eax will be the score for the given series 
; else eax = -1.
; assumption: the board is valid and rearanged
; parameters: moveSeriesPtr, rearrangedBoardPtr, numRows, numCols
checksolved PROC
	i_NumCols = 8
	i_NumRows =  i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_MoveseriesPtr = i_RearrangedBoardPtr + 4

	moveNum = - 4
	currentScore = moveNum - 4

	push ebp
	mov ebp, esp
	 
	push 0	  ; init moveNum to 0
	push 0	  ; init currentScore to 0
	push esi
	push ebx
	push ecx

	; esi is the pointer to the current cell on the board, initialize it to the first cell
	mov esi, [ebp + i_RearrangedBoardPtr]
	mov eax, 0

	; main loop that makex a move accoridng to the moveseries and checks the values of the cell stepped on
	makeMove:
		mov ebx, [ebp + i_MoveseriesPtr]
		add ebx, [ebp + moveNum]				;current move index - initialized to 0

		movsx ebx, byte PTR [ebx] 				;get the current move value

		cmp ebx, ';'							;if all moves done (reached ';') end game
		je endOfMoves
	
		add esi, ebx							;move esi position according to move value
		inc byte PTR [ebp + moveNum]			;increment move counter
		cmp esi, lastCellOnBoard				;if passed the last cell on the board => error
		jg passedLastCellOnBoard

	; check the value of the current cell
	checkCurrCell:
		cmp BYTE PTR [esi], 'E'
		je E							;jump if the cell is 'E'
		cmp BYTE PTR [esi], 'S'
		je S							;jump if the cell is 'S'
	
		; if the current cell holds a digit add its value to score and make another move
		digit:
			movsx ecx, BYTE PTR [esi]		; add the value in current cell to score
			add [ebp + currentScore], ecx
			jmp makeMove					; make another move

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
			jne outOfMoves							; if on the last move esi != lastCellOnBoard then the series given is not good
			mov eax, [ebp + currentScore]			; else (last cell reached on last move) => player won => save score in eax
			jmp endOfProc

		; if the player has passed the last cell on the board 
		; or 
		; the player has run out of moves before reaching the last cell on the board
		outOfMoves:
		passedLastCellOnBoard:
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
; assumption: the board is valid and rearanged
; parameters: moveSeriesPtr, nomoves, rearrangedBoardPtr, numRows, numCols
findshortseries PROC
	i_NumCols = 8
	i_NumRows = i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_Nomoves = i_RearrangedBoardPtr + 4
	i_MoveseriesPtr = i_Nomoves + 4

	lengthOfSeries = -4

	push ebp
	mov ebp, esp
	push 1			; init lengthOfSeries to 1
	push ebx

	; init moveSeries 
	mov ebx, [ebp + i_MoveseriesPtr]
	mov [ebx], byte PTR 1
	inc ebx
	mov [ebx], byte PTR ';'

	; main loop that generates moves and checks if they solve the board
	mainLoop:
		push [ebp + i_MoveseriesPtr]
		push [ebp + i_RearrangedBoardPtr]
		push [ebp + i_NumRows]
		push [ebp + i_NumCols]
		call checksolved
		cmp eax, -1
		jne solved

		push [ebp + i_MoveseriesPtr]
		push [ebp + lengthOfSeries]
		call nextMove
		cmp eax, 1
		je incSeriesLength
		LOOP mainLoop
	
		; nextMove has returned eax = 1 => we need to increment the seriesLength and add 1 to the end
		incSeriesLength:
			; check if we haven't reach the maximum series length (nomoves)
			mov	ebx, [ebp + i_Nomoves]
			cmp ebx, [ebp + lengthOfSeries]
			je noPossibleSolution

			; add 1,';' to the end of the current series and increment the lengthOfSeries value
			mov ebx, [ebp + i_MoveseriesPtr]
			add ebx, [ebp + lengthOfSeries]
			mov [ebx], byte PTR 1
			inc ebx
			mov [ebx], byte PTR ';'
			inc byte PTR [ebp + lengthOfSeries]
			LOOP mainLoop

	solved:		
		jmp endOfProc
	
	noPossibleSolution:
		mov [ebp + i_MoveseriesPtr], SBYTE PTR -1
		mov eax, -1

	endOfProc:
		pop ebx
		mov esp, ebp
		pop ebp
		ret 20
findshortseries ENDP

; given a series of moves, calculate the score
; assumption: the board is valid and rearanged, the set of moves is valid and ends at the last cell
; parameters: moveSeriesPtr, rearrangedBoardPtr, numRows, numCols
writescore PROC
	i_NumCols = 8
	i_NumRows =  i_NumCols + 4
	i_RearrangedBoardPtr = i_NumRows + 4
	i_MoveseriesPtr = i_RearrangedBoardPtr + 4

	push ebp
	mov ebp, esp
	push esi
	push ebx
	push ecx

	mov eax, 0									; init the score to 0
	mov esi, [ebp + i_RearrangedBoardPtr]

	makeMove:
		mov ebx, [ebp + i_MoveseriesPtr]
		movsx ebx, byte PTR [ebx] 				; get the current move value
		cmp ebx, ';'							; if all moves done (reached ';') end game
		je endOfMoves

		add esi, ebx							; move esi position according to move value

	checkCurrCell:
		cmp BYTE PTR [esi], 'E'
		je E									; jump if the cell is 'E'
		cmp BYTE PTR [esi], 'S'
		je S									; jump if the cell is 'S'
	
		; if the current cell holds a digit add its value to score
		digit:
			movsx ebx, BYTE PTR [esi]
			add eax, ebx								; add the value in current cell to score
			inc DWORD PTR [ebp + i_MoveseriesPtr]		; move the moveseries pointer to the next move
			jmp makeMove								; else make another move

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

; ============================================================= Additional procedures =============================================================

; gets a pointer to the unput board, and copies it in moving order to a new place in memory 
; assumption: all values of the series are numbers between 1-6
; parameters: (boardPtr, rearangedBoardPtr, numRows, numCols, lastCellOnBoardPtr)
copyAndRearangeBoard PROC	
	o_lastCellOnBoardPtr = 8
	i_numCols = o_lastCellOnBoardPtr + 4
	i_NumRows = i_numCols + 4
	o_RearangedBoardPtr = i_NumRows  + 4
	i_BoardPtr = o_RearangedBoardPtr + 4
	
	currRow = -4		; the current row index being copied

	push ebp
	mov ebp, esp
	push 0				; initialize currRow value
	PUSHAD	

	; initialize esi (boardPtr) and edi (rearangedBoardPtr)
	mov esi, [ebp + i_BoardPtr]
	mov edi, [ebp + o_RearangedBoardPtr]

	; set esi to the bottom left cell
	; moveEsiToBottomLeft(boardPtr, numRows, numCols)
	push [ebp + i_BoardPtr]
	push [ebp + i_Numrows]
	push [ebp + i_Numcols]
	call moveEsiToBottomLeft

	; main loop - each itteration copies a single row from board to rearrangedBoard
	nextRow:
		; compare curr to board, if curr < board then finished copying
		; (*) it doesn't matter if the last row is even or odd, in both cases the first cell is not less than 'OFFSET board'
		cmp esi, [ebp + i_BoardPtr]
		jl endOfCopy

		; check parity of row index
		mov eax, [ebp + currRow]
		and eax, 1
		cmp eax, 1			; (eax = 1 => odd row number)
		je copyOddRowLabel

		copyEvenRowLabel:
			; copyEvenRow(numCols)
			push [ebp + i_numCols]
			call copyEvenRow
			jmp endOfRowCopy

		copyOddRowLabel:
			; copyOddRow(numCols)
			push [ebp + i_numCols]
			call copyOddRow
		
		endOfRowCopy:			
			inc BYTE PTR [ebp + currRow]
			jmp nextRow

	endOfCopy:
		; set lastCellOnBoard to be the address of the last cell
		mov eax, OFFSET rearrangedBoard
		add eax, SIZEOF rearrangedBoard
		sub eax, TYPE rearrangedBoard
		mov lastCellOnBoard, eax
	
	POPAD
	mov esp, ebp
	pop ebp
	ret 20
copyAndRearangeBoard ENDP

; procedure to copy an entire odd index row from esi to edi
; essumption: esi and edi are at the beginning of the source and destination rows
; parameters: (numCols)
copyOddRow PROC
	i_NumCols = 8
	push ebp

	mov ebp, esp
	push eax
	push ecx

	; jump to the end of the source row to copy backwards
	add esi, [ebp + i_NumCols]
	dec esi

	; cells are copied one by one because we don't know in advance the value of numCols
	mov ecx, [ebp + i_NumCols]
	copyCell:
		mov al, BYTE PTR [esi]
		mov [edi], al
		inc edi
		dec esi
		loop copyCell

	; set esi to the beginning of the row above
	sub esi, [ebp + i_NumCols]
	inc esi

	pop ecx
	pop eax
	mov esp, ebp
	pop ebp
	ret 4
copyOddRow ENDP

; procedure to copy an entire even index row from esi to edi
; essumption, esi and edi are at the beginning of the source and destination rows
; parameters: numCols
copyEvenRow PROC
	i_NumCols = 8
	
	push ebp
	mov ebp, esp
	push eax
	push ecx

	; cells are copied one by one because we don't know in advance the value of numCols
	mov ecx, [ebp + i_NumCols]
	copyCell:
		mov al, BYTE PTR [esi]
		mov [edi], al
		inc edi
		inc esi
		loop copyCell

	; set esi to the beginning of the row above
	mov eax, [ebp + i_NumCols]
	sub esi, eax
	sub esi, eax

	pop ecx
	pop eax
	mov esp, ebp
	pop ebp
	ret 4
copyEvenRow ENDP

; set ESI to point at the bottom left cell of the given board
; parameters: boardPtr, numRows, numCols
moveEsiToBottomLeft PROC
	i_NumCols = 8
	i_NumRows = i_NumCols + 4
	i_BoardPtr = i_NumRows + 4

	push ebp
	mov ebp, esp
	push ecx

	; esi += (numRows-1) * numCols
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

; increment ESI to the row above
; assumptions: rearrangedBoardPtr points to a valid board
; parameters: rearrangedBoardPtr, numCols
stepOnE PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumCols + 4

	push ebp
	mov ebp, esp
	push eax
	push ebx

	; to move esi to the cell above the current position we need to increment 
	; it's value by ((numCols -(currCell % numCols)) * 2) + 1.
	; calculate numCols - (currCell % numCols)
	push [ebp + i_RearrangedBoardPtr]
	push [ebp + i_NumCols]
	call calc_ESI_Mod_NumCols				; eax = (currCell % numCols)
	mov ebx, eax
	mov eax, [ebp + i_NumCols]
	dec eax
	sub eax, ebx							; eax = (numCols - 1) - (currCell % numCols)

	; calculate ((numCols -(esi % numCols)) * 2) + 1
	mov ebx, 2
	mul ebx				; eax = ((numCols -(currCell % numCols)) * 2)
	inc eax				; eax = ((numCols -(currCell % numCols)) * 2) + 1

	; move esi to row above on the board
	add esi, eax		; esi += ((numCols -(esi % numCols)) * 2) + 1

	pop ebx
	pop eax
	mov esp, ebp
	pop ebp
	ret 8
stepOnE ENDP

; decrement ESI to the row above
; assumptions: rearrangedBoardPtr points to a valid board
; parameters: rearrangedBoardPtr, numCols
stepOnS PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumRows + 4
	push ebp
	mov ebp, esp
	push eax
	push ebx
	push edx

	; to move esi to the cell below the current position we need to decrement 
	; it's value by ((currCell % numCols) * 2) + 1.
	;calculate ((esi % numCols) * 2) + 1
	push [ebp + i_RearrangedBoardPtr]
	push [ebp + i_NumCols]
	call calc_ESI_Mod_NumCols		; eax = (currCell % numCols)
	mov ebx, 2
	mul ebx							
	inc eax							; eax = ((currCell % numCols) * 2) + 1

	;move esi to row below on the board
	sub esi, eax					; esi -= ((currCell % numCols) * 2) + 1

	pop edx
	pop ebx
	pop eax
	mov esp,ebp
	pop ebp
	ret 8 
stepOnS ENDP

; calculates esi (currentCell) modulos numCols  => currCell%numCols, returns the value in eax register
; parameters: rearrangedBoardPtr, numCols
calc_ESI_Mod_NumCols PROC
	i_NumCols = 8
	i_RearrangedBoardPtr = i_NumCols + 4
	push ebp
	mov ebp, esp
	push ebx
	push edx

	mov edx, 0
	mov eax, esi
	sub eax, [ebp + i_RearrangedBoardPtr]
	mov ebx, [ebp + i_NumCols]
	div ebx
	mov eax, edx				; eax = (currCell % numCols)

	pop edx
	pop ebx
	mov esp, ebp
	pop ebp
	ret 8
calc_ESI_Mod_NumCols ENDP

; print the results to the console
; assumption: moveseries must end with ';'
; parameters: moveseriesPtr, score
.data
	comma BYTE ", " , 0
	moveseriesStr BYTE "moveseries: ", 0
	scoreStr BYTE "score: " , 0

.code
printResults PROC
	i_Score = 8
	i_MoveSeriesPtr = i_Score + 4
	
	push ebp
	mov ebp, esp
	push eax
	push edx
	push esi

	; esi points to the current value in moveseries
	mov esi, [ebp + i_MoveSeriesPtr]

	; print "moveseries: "
	mov edx, OFFSET moveseriesStr
	call writeString
	
	 ; check if the series is valid (valid iff first value in moveseries not -1)
	 mov eax, [ebp + i_MoveSeriesPtr]
	 movsx eax, SBYTE PTR [eax]
	 cmp eax, -1
	 jnz printMove 
	 ; if score == -1 print "-1" and skip the printMove loop
	 call writeInt
	 jmp endOfMoveSeriesPrint

	; print the moveseries until reaching ';' (the first move can't be ';' so we print it without checking first)
	; each loop prints the current value and checks if the next one is ';'
	printMove:		
		movsx eax, SBYTE PTR [esi]
		call writeDec
		inc esi
		cmp [esi], byte PTR ';'
		jz endOfMoveSeriesPrint			; if the next moveseries valus is ';' we have reached the end of moveseries
		mov edx, OFFSET comma			; else print ", " and loop to print the next value
		call writeString
		jmp printMove

	; when finished printing the moveseries go down to the next row
	endOfMoveSeriesPrint:
	call CRLF	
	
	; print score
	mov edx, OFFSET scoreStr
	call writeString
	mov eax, [ebp + i_Score]
	call writeInt
	call CRLF

	pop esi
	pop edx
	pop eax
	mov esp, ebp
	pop ebp
	ret 8
printResults ENDP

END myMain
      subroutine HECTOR
     $(STRING,LENGTH,COUNT,PRINT,UNIT,SPACE)
C     Rudolf Loeser, 1979 Jan 29
C---- Prints "LENGTH" characters of "STRING" with Giant-Letters,
C     "COUNT" characters per group, on coded file "UNIT".
C     ("STRING" must be a character variable.)
C---- If UNIT .le. 0, then HECTOR does nothing.
C---- A group of Giant-Letters is 11 print-positions high, and
C     up to 127 print-positions wide.
C---- If the Giant-Letter image of a particular set of "COUNT"
C     characters is longer than 127 print-positions, then the
C     excess portion of the image is discarded. (This is
C     enforced through "LIMIT" in subroutine ARES.)
C---- "SPACE" is the carriage control character to be used to
C     print the first line of the Giant-Letter image.
C     If SPACE .eq. '1', then printing will start on a new page
C     after every 5 Giant-Letter groups.
C---- "PRINT" is used to designate the printing element from
C     which the Giant-Letters are constructed.
C     If PRINT .eq. 0: use '$';
C     if PRINT .gt. 0: use the character being imaged;
C     if PRINT .lt. 0: use 'O' over 'X' over '+'.
C     !DASH
      save
C     !DASH
      integer COUNT, I, IE, IS, LENGTH, LINES, PCOUNT, PRINT, UNIT
      character CARIAG*1, PARRAY*1, SPACE*1, STRING*(*)
C     !DASH
      external  GLAUCON, PARIS
      intrinsic min
C
      dimension CARIAG(3), PARRAY(3)
C
C     !BEG
      if(UNIT.gt.0) then
        LINES = LENGTH/COUNT
        if((LENGTH-LINES*COUNT).gt.0) then
          LINES = LINES+1
        end if
C
        IE = 0
C----   Loop over all Giant-Letter groups
        do 101 I = 1,LINES
          call GLAUCON (PRINT,PCOUNT,PARRAY,CARIAG,SPACE,I)
          IS = IE+1
          IE = min(IE+COUNT,LENGTH)
          call PARIS (STRING(IS:IE),COUNT,PARRAY,CARIAG,PCOUNT,
     $                PRINT,UNIT)
C         (Blank print line between groups)
          write (UNIT,100)
  100     format(' ')
  101   continue
      end if
C     !END
C
      return
      end

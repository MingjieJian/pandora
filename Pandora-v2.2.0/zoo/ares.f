      subroutine ARES
     $(GROUP,COUNT,P,PRINT,LINUM,LINE)
C     Rudolf Loeser, 1979 Jan 29
C---- Assembles "LINE" "LINUM" of the Giant-Letter image of the string
C     in "GROUP", length "COUNT", using the printing element "P".
C     !DASH
      save
C     !DASH
      integer COUNT, I, IE, IS, LIMIT, LINUM, PRINT, WIDTH
      character BLANK*1, GROUP*(*), IMAGE*16, LINE*(*), P*1, Q*1
C     !DASH
      external  GIANT
      intrinsic min
C
      data BLANK /' '/
      data LIMIT /127/
C
C     !BEG
      LINE = BLANK
      Q    = P
C
      IS = 0
      do 100 I = 1,COUNT
        if(PRINT.gt.0) then
          Q = GROUP(I:I)
        end if
C
        call GIANT (GROUP(I:I),LINUM,Q,WIDTH,IMAGE)
C
        IE = min(IS+WIDTH,LIMIT)
        IS = min(IS+1,LIMIT)
        LINE(IS:IE) = IMAGE(1:WIDTH)
        IS = IE+1
C
        if(IS.gt.LIMIT) goto 101
  100 continue
C
  101 continue
C     !END
C
      return
      end

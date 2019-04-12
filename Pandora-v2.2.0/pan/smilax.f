      subroutine SMILAX
     $(NO,MSFQR,MSFQM,MSFRT,MSFGR,LCOW)
C
C     Rudolf Loeser, 1989 Jan 30
C---- Prints Source Function Methods summary.
C     !DASH
      save
C     !DASH
      integer LCOW, MSFGR, MSFQM, MSFQR, MSFRT, NO
      character BLANK*1, COM*30
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('SMILAX')
C     !BEG
      if(NO.gt.0) then
        if(LCOW.gt.0) then
          COM = '(without CO-lines wavelengths)'
        else
          COM = BLANK
        end if
C
        call LINER (2,NO)
        write (NO,100) COM,MSFQR,MSFQM,MSFRT,MSFGR
  100   format(' ','Source Function Methods summary:',3X,'QR-direct',3X,
     $             'QR-mapped',10X,'RT',10X,'GR'/
     $         ' ',A30,2X,4I12)
      end if
C     !END
      call BYE ('SMILAX')
C
      return
      end

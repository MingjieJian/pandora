      subroutine RUBEES
     $(NO,N,Z,TE,XNE,HND,ZT,DTE,R1N)
C
C     Rudolf Loeser, 2001 Nov 20
C---- Optional supplementary input printout.
C     !DASH
      save
C     !DASH
      real*8 DTE, HND, R1N, TE, XNE, Z, ZT
      integer I, LINES, N, NO
C     !DASH
      external SHIM, LINER, ABJECT, HI, BYE
C
C               Z(N), TE(N), XNE(N), HND(N), ZT(N), DTE(N)
      dimension Z(*), TE(*), XNE(*), HND(*), ZT(*), DTE(*)
C
      call HI ('RUBEES')
C     !BEG
      write (NO,100)
  100 format(' ','Supplemental ATMOSPHERE data printout, to show ',
     $           'more figures.',51X,'(Option ZPRNT)'//
     $       ' ','DTE is the straight temperature gradient, ',
     $           'ZT is the logarithmic temperature gradient.')
      call LINER      (5,NO)
      write (NO,101)
  101 format(' ',29X,10X,'Z (km)',10X,'TE (K)',6X,'DTE (K/km)',
     $           8X,'ZT (/cm)',5X,'NE (/cm**3)',5X,'NH (/cm**3)')
      LINES = 20
C
      do 103 I = 1,N
        if(LINES.eq.45) then
          call ABJECT (NO)
          write (NO,101)
          LINES = 0
        end if
        call SHIM     ((I-1),5,NO)
        write (NO,102) I,Z(I),TE(I),DTE(I),ZT(I),XNE(I),HND(I)
  102   format(' ',24X,I5,1P6E16.8)
        LINES = LINES+1
  103 continue
      call LINER      (1,NO)
      write (NO,104) R1N
  104 format(' ',26X,'R1N',1PE16.8)
C     !END
      call BYE ('RUBEES')
C
      return
      end

      subroutine ZEUS
     $(LIN,LOPT,LOUT)
C     Rudolf Loeser, 1975 Oct 29
C---- Relates LOUT to LIN depending on LOPT.
C     (Used mainly to set up values of LUNs.)
C     !DASH
      save
C     !DASH
      integer LIN, LOPT, LOUT
C     !DASH
      external HI, BYE
C
      call HI ('ZEUS')
C     !BEG
      if(LOPT.gt.0) then
        LOUT = LIN
      else
        LOUT = 0
      end if
C     !END
      call BYE ('ZEUS')
C
      return
      end

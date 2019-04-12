      subroutine PAPA
     $(MSFT,IDDL,N,BOTH,JJ,KK)
C
C     Rudolf Loeser, 2004 May 11
C---- Establishes limit indices for combined FULL and DIRECT solutions.
C     (This is version 3 of PAPA.)
C     !DASH
      save
C     !DASH
      integer IDDL, JJ, KK, MSFT, N
      logical BOTH
C     !DASH
      external HI, BYE
C
      call HI ('PAPA')
C     !BEG
      BOTH = (MSFT.eq.0).and.(IDDL.lt.N)
C
      if(BOTH) then
        JJ = IDDL-1
        KK = IDDL
      else
        JJ = N
        KK = 1
      end if
C     !END
      call BYE ('PAPA')
C
      return
      end

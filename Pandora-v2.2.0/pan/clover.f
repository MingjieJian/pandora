      subroutine CLOVER
     $(MBD,BDJ,BDR,BDQ,BDIJ)
C
C     Rudolf Loeser, 1974 Dec 13
C---- Selects the desired set of B-ratios.
C     !DASH
      save
C     !DASH
      real*8 BDIJ, BDJ, BDQ, BDR
      integer MBD, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  MOVE1, HALT, HI, BYE
C
C               BDJ(N,NL), BDR(N,NL), BDQ(N,NL), BDIJ(N,NL)
      dimension BDJ(*),    BDR(*),    BDQ(*),    BDIJ(*)
C
      call HI ('CLOVER')
C     !BEG
      if(MBD.eq.0) then
        call MOVE1 (BDJ, (N*NL), BDIJ)
      else if(MBD.eq.1) then
        call MOVE1 (BDR, (N*NL), BDIJ)
      else if(MBD.eq.2) then
        call MOVE1 (BDQ, (N*NL), BDIJ)
      else
        write (MSSLIN(1),100) MBD
  100   format('MBD =',I12,', which is neither 0, 1, nor 2.')
        call HALT  ('CLOVER', 1)
      end if
C     !END
      call BYE ('CLOVER')
C
      return
      end

      subroutine TAFF
     $(KODE,TBEG,TEND)
C
C     Rudolf Loeser, 1989 Jul 06
C---- Accumulates Operator calculation statistics:
C     KODE = 1 for "Lambda",
C          = 2 for "Phi".
C
C     (This is version 2 of TAFF.)
C     !DASH
      save
C     !DASH
      real*8 DT, TBEG, TEND
      integer KODE
C     !COM
C---- WEITIM      as of 1990 Sep 05
      integer     KALLAM,KALPHI
      real*8      TIMLAM,TIMPHI
      common      /WEITIM1/ KALLAM,KALPHI
      common      /WEITIM2/ TIMLAM,TIMPHI
C     Weight matrix timing data:
C     1) KALLAM   - number of "Lambda-1" operator calculations;
C        TIMLAM   - total time (sec) for (1) calls.
C     2) KALPHI   - number of "Phi" operator calculations;
C        TIMPHI   - total time (sec) for (2) calls.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('TAFF')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT ('TAFF', 1)
      end if
C
      DT = TEND-TBEG
      if(KODE.eq.1) then
        TIMLAM = TIMLAM+DT
        KALLAM = KALLAM+1
      else
        TIMPHI = TIMPHI+DT
        KALPHI = KALPHI+1
      end if
C     !END
      call BYE ('TAFF')
C
      return
      end

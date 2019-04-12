      subroutine VISE
     $(PTAU,N,W,M,WW)
C
C     Rudolf Loeser, 1991 Sep 25
C---- Sets up WW, a running average of PTAU, for DAISY.
C     (This is version 3 of VISE.)
C     !DASH
      save
C     !DASH
      real*8 PTAU, W, WW
      integer M, N
      logical GOOD
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external RUNAVE, HALT, HI, BYE
C
C               PTAU(N), WW(N), W(M)
      dimension PTAU(*), WW(*), W(*)
C
      call HI ('VISE')
C     !BEG
      call RUNAVE (PTAU,N,W,M,WW,GOOD)
C
      if(.not.GOOD) then
        write (MSSLIN(1),100) N,M
  100   format('N =',I12,' and M =',I12,', bad values for computing ',
     $         'running average.')
        call HALT ('VISE',1)
      end if
C     !END
      call BYE ('VISE')
C
      return
      end

      subroutine ELATHA
     $(N,RMASS,HN1,FC,KILROY)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes FC for FRANCIS.
C     !DASH
      save
C     !DASH
      real*8 CON, FAC, FC, HN1, RMASS
      integer N
      logical KILROY
C     !DASH
      external MOVE1, CONMUL, HI, BYE
C
C               HN1(N), FC(N)
      dimension HN1(*), FC(*)
C
      data FAC /3.462D-7/
C
      call HI ('ELATHA')
C     !BEG
      if(KILROY) then
        KILROY = .false.
C
        CON = FAC*RMASS
        call MOVE1  (HN1, N, FC)
        call CONMUL (CON, FC, N)
      end if
C     !END
      call BYE ('ELATHA')
C
      return
      end

      subroutine PHLOX
     $(ILI,CHOP,CHLIM,N,T,PTAU)
C
C     Rudolf Loeser, 1991 Sep 25
C---- Sets up PTAU for DAISY.
C     PTAU = 1 means: use some of RHOJ; and
C     PTAU = 0 means: don't use any RHOJ.
C     (This is version 2 of PHLOX.)
C     !DASH
      save
C     !DASH
      real*8 CHLIM, CHOP, DL, ONE, PTAU, T, ZERO
      integer I, IC, ILI, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  NOTMORE, SET1, ZERO1, HI, BYE
      intrinsic abs
C
C               T(N), PTAU(N)
      dimension T(*), PTAU(*)
C
      call HI ('PHLOX')
C     !BEG
      call ZERO1       (PTAU, N)
C
      if((ILI.ge.1).and.(ILI.le.N)) then
        call SET1      (PTAU, ILI, ONE)
      else
C
        if(CHOP.gt.ZERO) then
          call NOTMORE (T, N, CHOP, IC)
          call SET1    (PTAU, IC, ONE)
        end if
C
        DL = abs(CHLIM)
        if(DL.ne.ZERO) then
          PTAU(1) = ONE
          do 100 I = 2,N
            if((T(I)-T(I-1)).lt.DL) then
              PTAU(I) = ONE
            end if
  100     continue
        end if
      end if
C     !END
      call BYE ('PHLOX')
C
      return
      end

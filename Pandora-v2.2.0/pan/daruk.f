      subroutine DARUK
     $(IMAGE,RAT,PRAT,N,KS,PP)
C
C     Rudolf Loeser, 1991 Jun 07
C---- Enters PRAT into the plot image, for IDATH.
C     !DASH
      save
C     !DASH
      real*8 HALF, ONE, PRAT, RAT, THREE, X1, X2
      integer I, ILAST, KS, N
      character IMAGE*(*), P*1, PN*1, PP*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external  CNVCHRL, DINI, KLINEC, HI, BYE
      intrinsic sign
C
C               RAT(N), PRAT(N)
      dimension RAT(*), PRAT(*)
C     !EJECT
C
      call HI ('DARUK')
C     !BEG
      call CNVCHRL      (PP,PN)
C
      ILAST = 1
      call DINI         (RAT(ILAST),PP,PN,P,KS)
C
  100 continue
        do 101 I = (ILAST+1),N
          if(sign(ONE,RAT(I-1)).eq.sign(ONE,RAT(I))) then
            X1 = I-1
            X2 = I
            call KLINEC (IMAGE,X1,PRAT(I-1),X2,PRAT(I),P,1)
          else
            ILAST = I-1
            go to 102
          end if
  101   continue
        go to 103
C
  102   continue
          X1 = ILAST
          X2 = X1+HALF
          call KLINEC   (IMAGE,X1,PRAT(ILAST),X2,THREE,P,1)
          ILAST = ILAST+1
          X1 = X2
          X2 = ILAST
          call DINI     (RAT(ILAST),PP,PN,P,KS)
          call KLINEC   (IMAGE,X1,THREE,X2,PRAT(ILAST),P,1)
        if(ILAST.lt.N) go to 100
C
  103 continue
C     !END
      call BYE ('DARUK')
C
      return
      end

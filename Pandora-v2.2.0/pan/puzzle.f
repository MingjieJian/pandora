      subroutine PUZZLE
     $(DL,XHZ,K,W)
C
C     Rudolf Loeser, 1973 Apr 20
C---- Computes Equivalent Width for a half-profile.
C     !DASH
      save
C     !DASH
      real*8 DL, TWO, W, WX, XHZ, ZERO
      integer I, K, LGT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external PLUSD, ZERO1, BUSH, DIVIDE, HI, BYE
C
C               W(K), DL(K), XHZ(K)
      dimension W(*), DL(*), XHZ(*)
C
      call HI ('PUZZLE')
C     !BEG
      call PLUSD        (XHZ,1,K,LGT)
      if(LGT.lt.K) then
        call ZERO1      (W,K)
      else
        call BUSH       (DL,1,XHZ,1,W,1,K)
        do 100 I = 1,K
          if(XHZ(I).eq.ZERO) then
            W(I) = ZERO
          else
            call DIVIDE (W(I),XHZ(I),WX)
            W(I) = TWO*(DL(I)-WX)
          end if
  100   continue
      end if
C     !END
      call BYE ('PUZZLE')
C
      return
      end

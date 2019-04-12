      subroutine BRIDE
     $(W,EI,WP,EIP,TC,A,F,FUNC)
C
C     Rudolf Loeser, 1974 Nov 25
C---- Computes an auxiliary function for color temperature calculation.
C     !DASH
      save
C     !DASH
      real*8 A, B, CON, EI, EIP, EXPT, F, FUNC, HNUKT, ONE, R, SEF, TC,
     $       THREE, W, WP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external PROD, RIGEL, QEXP1, HI, BYE
C
      call HI ('BRIDE')
C     !BEG
      call RIGEL (6,CON)
      R = CON/(W**3)
      A = (EIP-EI)/((ONE-W/WP)*R)
C
      call PROD  (TC,W,2,HNUKT,EXPT)
      call QEXP1 (HNUKT,EXPT,1,SEF)
      B = (EXPT/SEF)*(THREE-HNUKT/SEF)
C
      FUNC = A+B
C     !END
      call BYE ('BRIDE')
C
      return
      end

      subroutine THORN
     $(TE,XLM,CPI,H,BD,TN,SEF,XN,XD)
C
C     Rudolf Loeser, 1974 May 28
C---- Computes terms for ROSE.
C     !DASH
      save
C     !DASH
      real*8 BD, CPI, ES, H, HNUKT, ONE, RAT, SEF, TE, TN, XD, XLM, XN
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, PROD, HI, BYE
C
      call HI ('THORN')
C     !BEG
      call PROD   (TE,XLM,2,HNUKT,ES)
      call DIVIDE ((H*CPI),ES,RAT)
      XN = RAT*SEF
      XD = RAT*(BD-TN)
C     !END
      call BYE ('THORN')
C
      return
      end

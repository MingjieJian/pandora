      subroutine DINI
     $(VAL,PP,PN,P,KS)
C
C     Rudolf Loeser, 1991 Jun 07
C---- Chooses a plot character for DARUK.
C     !DASH
      save
C     !DASH
      real*8 VAL, ZERO
      integer KS
      character P*1, PN*1, PP*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
C
      call HI ('DINI')
C     !BEG
      if(VAL.ge.ZERO) then
        P = PP
      else
        P  = PN
        KS = 1
      end if
C     !END
      call BYE ('DINI')
C
      return
      end

      subroutine NUGA
     $(NSL,XNUC,XNU,AEW)
C
C     Rudolf Loeser, 2003 Apr 22
C---- Computes level edges (nm).
C     !DASH
      save
C     !DASH
      real*8 AEW, CON71, DIV, XNU, XNUC, ZERO
      integer I, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
C               XNU(NSL), XNUC(NSL), AEW(NSL)
      dimension XNU(*),   XNUC(*),   AEW(*)
C
      call HI ('NUGA')
C     !BEG
      call RIGEL (71, CON71)
      do 100 I = 1,NSL
        DIV = XNUC(I)-XNU(I)
        if(DIV.le.ZERO) then
          AEW(I) = ZERO
        else
          call DIVIDE (CON71, DIV, AEW(I))
        end if
  100 continue
C     !END
      call BYE ('NUGA')
C
      return
      end

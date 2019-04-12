      subroutine NOAM
     $(NSL,KXNUC,XNUK,WNUK,XNUC,WNUC)
C
C     Rudolf Loeser, 2006 Apr 06
C---- Sets up defaults for XNUC and WNUC.
C     (This is version 3 of NOAM.)
C     !DASH
      save
C     !DASH
      real*8 CON19, CON21, WNUC, WNUK, XNUC, XNUK, ZERO
      integer I, KXNUC, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, HI, BYE
C
C               XNUC(NSL), WNUC(NSL)
      dimension XNUC(*),   WNUC(*)
C
      call HI ('NOAM')
C     !BEG
      call RIGEL (19, CON19)
      call RIGEL (21, CON21)
C
      KXNUC = 0
C
      do 100 I = 1,NSL
        if(XNUC(I).eq.ZERO) then
          if(WNUC(I).ne.ZERO) then
            XNUC(I) = CON21*WNUC(I)
          else
            XNUC(I) = XNUK
          end if
        else
          KXNUC = 1
        end if
        if(WNUC(I).eq.ZERO) then
          WNUC(I) = CON19*XNUC(I)
        else
          KXNUC = 1
        end if
  100 continue
C     !END
      call BYE ('NOAM')
C
      return
      end

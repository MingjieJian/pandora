      subroutine TULLEM
     $(ITAU,XLM,XKH,XKA,OPAC,DMPI)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Computes normalized highest H Ly lines absorption.
C     !DASH
      save
C     !DASH
      real*8 OPAC, SLM, W1, W2, XKA, XKH, XLM
      integer ITAU, LUEO, NQ
      logical DMPI
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LANKI, HI, BYE
C
      data SLM /-1.D0/
C
      call HI ('TULLEM')
C     !BEG
      if(XLM.ne.SLM) then
        SLM = XLM
        call LANKI (SLM,W1,W2,NQ)
      end if
C
      OPAC = XKH*W1+XKA*W2
C
      if(DMPI) then
        write (LUEO,100) NQ,ITAU,XKH,W1,XKA,W2,OPAC
  100   format(' ','(NQ =',I4,')',66X,'Highest H Ly lines absorption ',
     $             'at i =',I5,5X,'@@@@@'/1P,
     $         ' ','KH =',E16.8,5X,'W1 =',E16.8,5X,'KKA =',E16.8,5X,
     $             'W2 =',E16.8,5X,'Opac =',E16.8,1X,'@@@')
      end if
C     !END
      call BYE ('TULLEM')
C
      return
      end

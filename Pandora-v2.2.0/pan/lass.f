      subroutine LASS
     $(XLM,ITAU,EMA,EMB,OPAC,EMIT,DMPI)
C
C     Rudolf Loeser, 2003 Jan 07
C---- Computes normalized highest H Ly lines emission.
C     !DASH
      save
C     !DASH
      real*8 EMA, EMB, EMIT, OPAC, SLM, W1, W2, XLM
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
      call HI ('LASS')
C     !BEG
      if(XLM.ne.SLM) then
        SLM = XLM
        call LANKI (SLM, W1, W2, NQ)
      end if
C
      EMIT = OPAC*(EMA*W1+EMB*W2)
C
      if(DMPI) then
        write (LUEO,100) NQ,ITAU,EMA,W1,EMB,W2,OPAC,EMIT
  100   format(' ','(NQ =',I4,')',67X,'Highest H Ly lines emission ',
     $             'at i =',I5,' ##########'/1P,
     $         ' ','EMA =',E16.8,5X,'W1 =',E16.8,5X,'EMB =',E16.8,5X,
     $             'W2 =',E16.8/
     $         ' ','OPAC =',E16.8,10X,'EMIT =',E16.8,63X,'##########')
      end if
C     !END
      call BYE ('LASS')
C
      return
      end

      subroutine BRECON
     $(I,IDEDP,TE,PGS,XIH,XIM,XIP,AMAS,ABUN,XK,VCM)
C
C     Rudolf Loeser, 1998 Oct 28
C---- Prints, for PEGEL.
C     (This is version 2 of BRECON.)
C     !DASH
      save
C     !DASH
      real*8 ABUN, AMAS, PGS, TE, VCM, XIH, XIM, XIP, XK
      integer I, IDEDP, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('BRECON')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) I,IDEDP,TE,PGS,XIH,XIM,XIP,AMAS,ABUN,XK,VCM
  100 format(' ','Details from calculation of d-matrix, ',
     $           'for depth #',I4/
     $       ' ','(Option AMDDMP = on, and IDEDP =',I3,')'//
     $       ' ','TEMP =',1PE11.3,', PGAS =',E11.3/
     $       ' ','XIH (= XION) =',E11.3,', XIM =',E11.3,
     $           ', XIP =',E11.3/
     $       ' ','AMAS =',E11.3,', ABUN =',E11.3,', XK =',E11.3,
     $           ', VCM =',E11.3)
C     !END
      call BYE ('BRECON')
C
      return
      end

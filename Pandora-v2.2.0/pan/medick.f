      subroutine MEDICK
     $(N,AN1,AN1S,STKFN,X,A,PHI,OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints details, for AMETH.
C     (This is version 3 of MEDICK.)
C     !DASH
      save
C     !DASH
      real*8 A, AN1, AN1S, OPAC, PHI, SCAT, STKFN, X
      integer LUEO, N
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
      call HI ('MEDICK')
C     !BEG
      call LINER (1,LUEO)
      write (LUEO,100) N,AN1,AN1S,X,A,STKFN,PHI,OPAC,SCAT
  100 format(' ','N =',I3,' core;   AN1 =',1PE16.8,5X,' sANk =',E16.8,
     $           5X,'    x =',E16.8,5X,'    a =',E16.8/
     $       ' ',13X,'STKFN =',E16.8,5X,'  phi =',E16.8,5X,'OPACN =',
     $           E16.8,5X,'SCATN =',E16.8)
C     !END
      call BYE ('MEDICK')
C
      return
      end

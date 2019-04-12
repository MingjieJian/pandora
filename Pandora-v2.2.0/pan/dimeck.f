      subroutine DIMECK
     $(N,AN1,AN1S,STKFN,X,DR,TERM,RAYS,OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints details, for AMETH.
C     !DASH
      save
C     !DASH
      real*8 AN1, AN1S, DR, OPAC, RAYS, SCAT, STKFN, TERM, X
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
      call HI ('DIMECK')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) N,AN1,AN1S,X,DR,TERM,RAYS,STKFN,OPAC,SCAT
  100 format(' ','N =',I3,' wing;   AN1 =',1PE16.8,5X,' sANk =',E16.8,
     $           5X,'    x =',E16.8,5X,'   DR =',E16.8/
     $       ' ',69X,' TERM =',E16.8,5X,' RAYS =',E16.8/
     $       ' ',13X,'STKFN =',E16.8,33X,'OPACN =',E16.8,5X,
     $           'SCATN =',E16.8)
C     !END
      call BYE ('DIMECK')
C
      return
      end

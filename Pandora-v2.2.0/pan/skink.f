      subroutine SKINK
     $(AMASS,ABD,QNAME,QELSM,QATOM,LINE,IZZ,NO)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Prints atomic constants, for ATOM.
C     (This is version 2 of SKINK.)
C     !DASH
      save
C     !DASH
      real*8 ABD, AMASS
      integer IZZ, NO
      character ANAME*10, LINE*120, QATOM*8, QELSM*8, QIONM*8, QNAME*8,
     $          RIONM*10, RNAME*10
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (QEST( 1),QIONM)
C     !DASH
      external LINER, RIGHT, VARUS, VARONA, CRY, HI, BYE
C
      call HI ('SKINK')
C     !BEG
      if(QATOM.ne.'!NONAME!') then
        call RIGHT (QATOM, ANAME, 10)
        call LINER (3, NO)
        write (NO,100) ANAME
  100   format(' ',23X,'Ion model "name"',10X,A10)
      end if
C
      call LINER   (3, NO)
      call RIGHT   (QNAME, RNAME, 10)
      call RIGHT   (QIONM, RIONM, 10)
      write (NO,101) RNAME,RIONM
  101 format(' ',29X,A10,10X,A10)
C
      call LINER   (1, NO)
      call VARUS   ('Atomic Mass',
     $              AMASS, LINE, IZZ, NO, 0)
      call VARUS   ('Abundance relative to Hydrogen',
     $              ABD, LINE, IZZ, NO, 0)
C
C---- The Hydrogen sermon
      call VARONA  (NO)
C---- Warning for built-in simulated lines is the background
      call CRY     (NO)
C     !END
      call BYE ('SKINK')
C
      return
      end

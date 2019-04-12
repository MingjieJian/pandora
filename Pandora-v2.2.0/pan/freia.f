      subroutine FREIA
     $(X,W)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Produces: Continuum Wavelengths Summaries
C               BETA Summary
C               Continuum Data Save File
C     (This is version 4 of FREIA.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IBETA, ICORC, IN, IS, IXCBL, JJARC, JJTE, JJZ, MOX
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(158),JJARC)
C     !DASH
      external LOLA, SHARI, ROMNEY, FAERI, RAISSA, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXCBL ),(IN( 2),IBETA ),(IN( 3),ICORC )
C
      call HI ('FREIA')
C     !BEG
C     (Get, and allccate, W allotment)
      call LOLA   (IN, IS, MOX, 'FREIA')
C
C---- Continuum Wavelengths Summary, Part 0 [ and Parts 1, 2 ?]
      call SHARI  (W(IXCBL), X(JJZ), X(JJTE))
C
C---- [ BETA Summary ?]
      call ROMNEY (X(JJTE), W(IBETA))
C
C---- [ Composite Line Opacity analysis ?]
      call FAERI  (W(IXCBL), X(JJARC), W(ICORC))
C
C---- [ Continuum Data Save File ?]
      call RAISSA (W(IXCBL), X(JJZ), X(JJTE))
C
C     (Give back W allotment)
      call WGIVE  (W, 'FREIA')
C     !END
      call BYE ('FREIA')
C
      return
      end

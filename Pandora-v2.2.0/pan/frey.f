      subroutine FREY
     $(XCBL,X,WAVES,IADRS,KTYPE,KTRU,MODE,KHED,PPRNT,CALLER)
C
C     Rudolf Loeser, 2004 Aug 23
C---- Reads the continuum data for the current wavelength, WAVES,
C     into the buffer XCBL, and does various initializations for the
C     calculations at this wavelength.
C     (This is version 2 of FREY.)
C     !DASH
      save
C     !DASH
      real*8 WAVES, X, XCBL
      integer IADRS, KHED, KTRU, KTYPE, MODE
      logical PPRNT
      character CALLER*(*)
C     !DASH
      external LETTER, BEECH, PARLOR, ISTUR, HI, BYE
C
      dimension X(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      call HI ('FREY')
C     !BEG
C---- Read
      call LETTER (IADRS, XCBL, WAVES, 0, CALLER)
C
C---- Decode KTYPE into KWKS (Kwack)
      call BEECH  (KTYPE)
C
C---- Set up print controls
      call PARLOR (X, XCBL, KTRU, MODE, KHED, PPRNT)
C
C---- Check for Hydrogen Lyman lines
      call ISTUR  (XCBL)
C     !END
      call BYE ('FREY')
C
      return
      end

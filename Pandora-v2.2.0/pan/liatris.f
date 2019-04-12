      subroutine LIATRIS
     $(KODE)
C
C     Rudolf Loeser, 1995 Mar 08
C---- Encodes a "time stamp" for Continuum Calculations.
C     (This is version 2 of LIATRIS.)
C     !DASH
      save
C     !DASH
      integer IOVER, ITHSL, KODE, LITER
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(24),LITER)
      equivalence (LEST(19),ITHSL)
C     !DASH
      external MASSAGE, HI, BYE
C
      call HI ('LIATRIS')
C     !BEG
      call MASSAGE (1,KODE,IOVER,ITHSL,LITER)
C     !END
      call BYE ('LIATRIS')
C
      return
      end

      subroutine LEONIA
     $(X,DL,K,ITYPE,KPRD,CORE,XCBL,WVS,JOPAC,JOPAT)
C
C     Rudolf Loeser, 1980 Apr 07
C---- Sets up a set of line background Continuum Data Block records,
C     for EUDOXIA.
C     (This is version 2 of LEONIA.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DAMP, DL, OM, WVS, X, XCBL
      integer ITYPE, JJOML, JJYCO, JOPAC, JOPAT, K, KODE, KPRD
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ(  3),JJOML)
C     !DASH
      external LIMPOPO, PUMICE, TRUDE, HI, BYE
C
      dimension X(*)
C
C               XCBL(Miklen), DL(K), WVS(K), JOPAC(Nopac), JOPAT(Nopac)
      dimension XCBL(*),      DL(*), WVS(*), JOPAC(*),     JOPAT(*)
C
      data KODE /-1/
C
      call HI ('LEONIA')
C     !BEG
C---- Get control parameters
      call LIMPOPO (DAMP, OM, X(JJYCO), X(JJOML))
C---- Set up wavelengths table
      call PUMICE  (CORE, DL, K, WVS)
C---- Set up and write out blocks
      call TRUDE   (X, XCBL, K, WVS, CORE, DAMP, ITYPE, KPRD, OM, KODE,
     $              JOPAC, JOPAT)
C     !END
      call BYE ('LEONIA')
C
      return
      end

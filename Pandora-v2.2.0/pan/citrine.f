      subroutine CITRINE
     $(X,IX,W,IW,XCBL,I)
C
C     Rudolf Loeser, 2005 Mar 25
C---- Supervises updating of background data for PRD.
C     (This is version 4 of CITRINE.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL
      integer I, IW, IX, KKBHS, KKBHSD, KKBHSN, KKBNMS, KKCAPP, KKMULT,
     $        KKOPAC, KKSCAT, KKSIGM, KKSIGS, KKZABS, KKZBDN, KKZBNM,
     $        KKZSCA, KKZSCR, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK( 7),KKSCAT)
      equivalence (KKK(16),KKBNMS)
      equivalence (KKK( 8),KKBHSN)
      equivalence (KKK( 9),KKBHSD)
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(35),KKZABS)
      equivalence (KKK(36),KKZSCA)
      equivalence (KKK(37),KKZSCR)
      equivalence (KKK(41),KKZBNM)
      equivalence (KKK(57),KKZBDN)
      equivalence (KKK(45),KKSIGS)
C
C---- KONOUT      as of 2004 Jan 08
      integer     KONLUN,KONLUR,KONLUD,KONHED
      common      /KONOUT/ KONLUN,KONLUR,KONLUD,KONHED
C     Logical output unit numbers for Continuum Calculations.
C             *** Initialized in PARLOR. ***
C
C     KONLUN: potential output unit number
C     KONLUR: unit number for regular output
C
C     KONLUD: =1 if dump output is authorized
C     KONHED: =1 if wavelength header has already been printed
C     .
C     !DASH
C     !EJECT
      external HAZY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      call HI ('CITRINE')
C     !BEG
      call HAZY (N, KONLUR, I,
     $           XCBL(KKMULT), XCBL(KKCAPP), XCBL(KKBNMS),
     $           XCBL(KKSIGS), XCBL(KKZABS), XCBL(KKZSCA),
     $           XCBL(KKZSCR), XCBL(KKZBNM), XCBL(KKZBDN),
     $           XCBL(KKOPAC), XCBL(KKSIGM), XCBL(KKSCAT),
     $           XCBL(KKBHSN), XCBL(KKBHSD), XCBL(KKBHS) )
C     !END
      call BYE ('CITRINE')
C
      return
      end

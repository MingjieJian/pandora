      subroutine HUILA
     $(K,CCHX,WRK,ICXDP,CALLER)
C
C     Rudolf Loeser, 1990 Nov 30
C---- Dump printout for upper-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 CCHX, WRK
      integer ICXDP, K, LUEO, NM
      character CALLER*(*)
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, COMPACT, LINER, PRAY, HI, BYE
C
C               WRK(NPQLM,NPQLM)
      dimension WRK(*)
C
      call HI ('HUILA')
C     !BEG
      call MESHED  (CALLER, 2)
      write (LUEO,100) NAMXED(K),BXED(K),AXED(K),CCHX,ICXDP,K
  100 format(' ','Dump output from upper-level charge exchange ',
     $           'calculation.  (Controlled by option CHXDMP)'//
     $       ' ','For ',A,5X,'B =',1PE14.6,5X,'A =',E14.6,
     $           5X,'CCHX =',E14.6,5X,'depth # ICXDP',I5,24X,I2//
     $       ' ','The matrix  delta(n,l):')
      NM = NPQMX-1
      call COMPACT (DELCHX, NPQMX, NPQLM, NM, WRK)
      call PRAY    (LUEO, WRK, NPQMX, NM)
      call LINER   (2, LUEO)
C     !END
      call BYE ('HUILA')
C
      return
      end

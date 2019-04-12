      subroutine CUDDLY
     $(KK,XK,GK,WAVES,IADRS,KTYPE)
C
C     Rudolf Loeser, 2005 Aug 18
C---- Dumps, for PRIDE.
C     !DASH
      save
C     !DASH
      real*8 GK, WAVES, XK
      integer I, IADRS, IPEX, KK, KTYPE, LUEO
C     !COM
C---- LYMALIM     as of 2005 Jul 08
      real*8      XKWAVU,XKWAVL
      common      /LYMALIM/ XKWAVU,XKWAVL
C     Wavelength limits (Angstroms) for "Lyman" calculation.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, MASHED, LINER, SHIM, HI, BYE
C
C               WAVES(KKX), IADRS(KKX), KTYPE(KKX), XK(KKX), GK(KKX)
      dimension WAVES(*),   IADRS(*),   KTYPE(*),   XK(*),   GK(*)
C
      call HI ('CUDDLY')
C     !BEG
      if((IPEX.lt.0).or.(IPEX.eq.12)) then
        call MESHED ('CUDDLY', 2)
        write (LUEO,100)
  100   format(' ','Setting up the augmented "Lyman" XK-table.'//
     $         ' ',25X,'wave',12X,'adrs',6X,'type',14X,'XK',14X,'GK')
        call LINER  (1, LUEO)
        write (LUEO,101) 0,XKWAVU
        call LINER  (1, LUEO)
        do 102 I = 1,KK
          write (LUEO,101) I,WAVES(I),IADRS(I),KTYPE(I),XK(I),GK(I)
  101     format(' ',I5,1PE24.16,I16,I10,2E16.8)
          call SHIM (I, 5, LUEO)
  102   continue
        call LINER  (1, LUEO)
        write (LUEO,101) 0,XKWAVL
        call MASHED ('CUDDLY')
      end if
C     !END
      call BYE ('CUDDLY')
C
      return
      end

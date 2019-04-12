      subroutine CAMINA
     $(XK,KK,KKX,KOLEV,XNUK,XNU)
C
C     Rudolf Loeser, 2005 Jul 07
C---- Computes KKX, the length of the extended XK table for the
C     Lyman-calculation of level KOLEV,
C     and
C     sets up wavelength limits.
C     !DASH
      save
C     !DASH
      real*8 DNU, FNU, XK, XNU, XNUK
      integer IPEX, KK, KKX, KL, KOLEV, KU, LUEO
      logical OK
C     !COM
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
C---- LYMALIM     as of 2005 Jul 08
      real*8      XKWAVU,XKWAVL
      common      /LYMALIM/ XKWAVU,XKWAVL
C     Wavelength limits (Angstroms) for "Lyman" calculation.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  ANGIE, RAYMOND, MESHED, MASHED, HI, BYE
      intrinsic max
C
C               XK(KK), XNU(NSL)
      dimension XK(*),  XNU(*)
C
      call HI ('CAMINA')
C     !BEG
      DNU = XNUK-XNU(KOLEV)
C
      FNU = DNU*XK( 1)
      call ANGIE    (FNU, XKWAVU)
      call RAYMOND  (XKWAVU, 0, KU)
C
      FNU = DNU*XK(KK)
      call ANGIE    (FNU, XKWAVL)
      call RAYMOND  (XKWAVL, 0, KL)
C
      OK = (KU.ge.KL).and.(KU.ne.0)
      if(OK) then
        KKX = max(KK, (KU-KL+1))
      end if
C
      if(((IPEX.lt.0).or.(IPEX.eq.12)).or.(.not.OK)) then
        call MESHED ('CAMINA', 2)
        write (LUEO,100) KOLEV,KK,KKX,XK(1),XK(KK),
     $                   XKWAVU,KU,XKWAVL,KL
  100   format(' ','KOLEV =',I3,5X,'KK =',I10,5X,'KKX =',I10,5X,
     $             'XK(1) =',F10.5,5X,'XK(KK) =',F10.5/
     $         ' ','XKWAVU =',1PE24.16,5X,'KU =',I8,2X,
     $             'XKWAVL =',  E24.16,5X,'KL =',I8)
        call MASHED ('CAMINA')
      end if
C     !END
      call BYE ('CAMINA')
C
      return
      end

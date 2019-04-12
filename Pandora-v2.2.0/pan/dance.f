      subroutine DANCE
     $(WAVLOG,NW,XL,XR,KODE)
C
C     Rudolf Loeser, 1978 Feb 23
C---- Determines abscissa limits, for DONATI.
C     !DASH
      save
C     !DASH
      real*8 ONE, WAVLOG, WL, WR, XL, XR, ZERO
      integer I, IPEX, KD, KODE, KU, LUEO, NW
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external BELOWD, ABOVED, ELCMAR, MESHED, MASHED, HI, BYE
C
C               WAVLOG(Nmkuse)
      dimension WAVLOG(*)
C
      call HI ('DANCE')
C     !BEG
      KODE = 0
C
      KU = 1
      do 100 I = 1,NW
        if(WAVLOG(KU).le.ZERO) then
          KU = KU+1
        end if
  100 continue
C
      KD = NW
      do 101 I = 1,NW
        if(WAVLOG(KD).le.ZERO) then
          KD = KD-1
        end if
  101 continue
C
      if(KD.gt.KU) then
        KODE = 1
C
        WL = WAVLOG(KU)
        WR = WAVLOG(KD)
        if(WL.gt.WR) then
          WL = WAVLOG(KD)
          WR = WAVLOG(KU)
        end if
        call BELOWD   (WL, ONE, XL)
        call ABOVED   (WR, ONE, XR)
C
        if((XR-XL).le.ONE) then
C----     These limits span only one decade - perhaps they should be
C         made even tighter
          call ELCMAR (XL, WL, WR, XR)
        end if
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.26)) then
        call MESHED   ('DANCE', 2)
        write (LUEO,102) KODE,KU,XL,WL,KD,WR,XR
  102   format(' ','Ordinates limits selection, KODE =',I2/1P,
     $         ' ','KU =',I10,', XL =',E16.8,', WL =',E16.8/
     $         ' ','KD =',I10,', WR =',E16.8,', XR =',E16.8)
        call MASHED   ('DANCE')
      end if
C     !END
      call BYE ('DANCE')
C
      return
      end

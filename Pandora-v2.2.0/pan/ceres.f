      subroutine CERES
     $(LYNC,WLYNC,XLYNC,NW,WAVES,XLTIT,EMINT)
C
C     Rudolf Loeser, 2005 Nov 04
C---- Picks out data for H Ly line normalization.
C     (This is version 2 of CERES.)
C     !DASH
      save
C     !DASH
      real*8 EMINT, WAVES, WHI, WLO, WLYNC, XLTIT, XLYNC
      integer I, KLYNF, KTYPE, LUEO, LYNC, NW
      logical WOK
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
      equivalence (LEST(81),KLYNF)
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 4),KTYPE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external BET, MESHED, MASHED, HI, BYE
C
C               WLYNC(KLYNF), XLYNC(KLYNF), WAVES(NW), EMINT(NW,L),
      dimension WLYNC(*),     XLYNC(*),     WAVES(*),  EMINT(NW,*),
C
C               XLTIT(NW)
     $          XLTIT(*)
C
      data WLO,WHI /1.0D3, 1.3D3/
C
      call HI ('CERES')
C     !BEG
      LYNC = 0
C
      if(KLYNF.gt.0) then
        do 101 I = 1,NW
          WOK = (WAVES(I).ge.WLO).and.(WAVES(I).le.WHI)
          if(WOK) then
            call BET (2, XLTIT(I))
            if(KTYPE.eq.27) then
              LYNC = LYNC+1
C
              if(LYNC.gt.KLYNF) then
                call MESHED ('CERES', 3)
                write (LUEO,100) LYNC,KLYNF
  100           format(' ','LYNC =',I12,'; KLYNF =',I12,
     $                     ', which is too large.'/
     $                 ' ','Calculation of FNRMLA, FNRMLB ',
     $                     'will be skipped.')
                call MASHED ('CERES')
                LYNC  = 0
                KLYNF = 0
                goto 102
              end if
C
              WLYNC(LYNC) = WAVES(I)
              XLYNC(LYNC) = EMINT(I,1)
            end if
          end if
  101   continue
      end if
C
  102 continue
C     !END
      call BYE ('CERES')
C
      return
      end

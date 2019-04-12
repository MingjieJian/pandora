      subroutine LEANDRA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1977 Jan 28
C---- Allocates scratch storage for LIGHT.
C     !DASH
      save
C     !DASH
      integer IN, IPEX, IS, J, KM, KML, L, LUEO, MUX, N, NKM,
     $        NNVP, NVP, NVX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(42),NVX)
C
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
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
C     !EJECT
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C     !DASH
      external WGET, WLCK, MESHED, MASHED, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('LEANDRA')
C     !BEG
      call WGET     (IS,  CALLER)
C
      NVP  = NVX+3
      NKM  = N*KM
      KML  = KM*LFBV
      NNVP = N*NVP
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+LI1LEN
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+NKM
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKM
      IN( 7) = IN( 6)+NNVP
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+KML*L
      IN(11) = IN(10)+KML
C
      IN(12) = IN(11)+KML
      IN(13) = IN(12)+LI2LEN
      MUX    = IN(13)+LI3LEN
C
      call WLCK     (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED ('LEANDRA', 2)
        write (LUEO,100) IS,KM,L,LFBV,NVP,NNVP,NKM,KML,MUX
  100   format(' ','IS =',I12,', KM =',I12,', L =',I12,', LFBV =',I12/
     $         ' ','NVP =',I12,', NNVP =',I12,', NKM =',I12,
     $             'KML =',I12,', MUX =',I12)
        write (LUEO,101) (IN(J),J=1,13)
  101   format(' ','IN =',10I12)
        call MASHED ('LEANDRA')
      end if
C     !END
      call BYE ('LEANDRA')
C
      return
      end

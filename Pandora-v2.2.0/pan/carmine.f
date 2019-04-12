      subroutine CARMINE
     $(NO,N,K,DL,COPTRN,BCTRN,GTN,DW,STRN,ICE,IU,IL,PHI,TNU,SNU)
C
C     Rudolf Loeser, 1980 Jul 07
C---- Dumps intermediates of line profile calculation.
C     (This is version 2 of CARMINE.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, COPTRN, DL, DW, GTN, PHI, SNU, STRN, TNU
      integer I, ICE, IL, IU, J, K, KIC, N, NO
      character FACELAB*10, TIT*4, TUT*24
C     !COM
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
C     !EJECT
      external  LOOT, ABJECT, LINER, TUMBLE, HI, BYE
      intrinsic min, max
C
C               COPTRN(N,KM), BCTRN(N,KM), TNU(N,KM), SNU(N,KM), DW(N),
      dimension COPTRN(N,*),  BCTRN(N,*),  TNU(N,*),  SNU(N,*),  DW(*),
C
C               PHI(N,KM), STRN(N,KM), DL(KM), GTN(N)
     $          PHI(N,*),  STRN(N,*),  DL(*),  GTN(*)
C
      dimension TIT(2)
C
      data TIT /'CRD ', 'PRD '/
C
      call HI ('CARMINE')
C     !BEG
      if(NO.gt.0) then
        call LOOT    (TUT)
        call TUMBLE  (LFB, FACELAB)
        KIC = min((max(ICE,0)),1)
C
        call ABJECT  (NO)
        write (NO,100) IU,IL,TIT(KIC+1),TUT,FACELAB
  100   format(' ','Intermediates for Line Profile calculation',2X,
     $             '(',I2,'/',I2,')',2X,A4,2X,A24,4X,A10)
        call LINER   (1, NO)
C
        do 103 J = 1,K
          call LINER (1, NO)
          write (NO,101) J,DL(J)
  101     format(' ','J=',I3,2X,'DL=',1PE12.4//
     $           ' ',15X,'COP',13X,'BC',12X,'GTN',14X,'S',12X,'PHI',
     $               12X,'TNU',12X,'SNU',13X,'DW')
          call LINER (1, NO)
C
          write (NO,102) (I,COPTRN(I,J),BCTRN(I,J),GTN(I),STRN(I,J),
     $                    PHI(I,J),TNU(I,J),SNU(I,J),DW(I),I=1,N)
  102     format(5(' ',I3,1P8E15.7/))
  103   continue
C
      end if
C     !END
      call BYE ('CARMINE')
C
      return
      end

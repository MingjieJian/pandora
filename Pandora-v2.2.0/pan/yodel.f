      subroutine YODEL
     $(N,NSL,Z,TE,TR,RABD,KSHEL,CQIN,JH1,JH2,HJ,FRS,PF,RABDL,
     $ IZZ,LINE,NO)
C
C     Rudolf Loeser, 2005 Oct 14
C---- Prints ion-specific depth-dependent data, for DABBLE.
C     !DASH
      save
C     !DASH
      real*8 CQIN, FRS, HJ, PF, RABD, RABDL, TE, TR, Z
      integer IB, IE, INC, IQSFS, IQUTR, IZZ, J, JH1, JH2, KKPFN, KKPRM,
     $        KKRAB, KKRBL, KSHEL, N, NO, NSL
      logical ALLK, KPF, KRB, KRL, KSHL, SPHR, TRIN
      character LINE*120, QLABEL*8, TIT*40
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ( 31),IQSFS)
C     !DASH
C     !EJECT
      external  ABJECT, PHOEBE, COTTON, FROGZ, KONSTD, FROGGY, FROGG,
     $          FRIG, LEAFY, LINER, HI, BYE
      intrinsic min
C
C               Z(N), CQIN(N), HJ(N), FRS(N), PF(N), RABD(N), RABDL(N),
      dimension Z(*), CQIN(*), HJ(*), FRS(*), PF(*), RABD(*), RABDL(*),
C
C               TR(N,NSL), TE(N)
     $          TR(N,*),   TE(*)
C
      call HI ('YODEL')
C     !BEG
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Ion-specific depth-dependent input tables.')
      call LINER  (2, NO)
C
      call PHOEBE (1, QLABEL)
      TRIN = IQUTR.gt.0
      KSHL = KSHEL.gt.0
      SPHR = IQSFS.gt.0
C
      call KONSTD (RABD,  1, N, RABD(1),  KRB)
      call KONSTD (RABDL, 1, N, RABDL(1), KRL)
      call KONSTD (PF,    1, N, PF(1),    KPF)
      ALLK = KRB.and.KRL.and.KPF
      if(ALLK.and.(.not.TRIN).and.(.not.KSHL)) then
        write (NO,101) RABD(1),RABDL(1),QLABEL,PF(1)
  101   format(' ','Abbreviated listing:'/
     $         ' ','   Abundance ratio all =',1PE14.6/
     $         ' ','   log10(Abundance ratio) all =',E14.6/
     $         ' ','   Partition function of ',A,' all =',E14.6)
      else
C     !EJECT
        IE = 0
  102   continue
          IB  = IE+1
          IE  = min(IE+8,N)
          INC = (IE-IB)+1
C
          call COTTON    (IB, IE, NO)
          call LINER     (1, NO)
          call FROGZ     ('Depth (km)',
     $                    Z(IB), INC, LINE, IZZ, NO)
          if(SPHR) then
            call FROGZ   ('Distance (radii)',
     $                    FRS(IB), INC, LINE, IZZ, NO)
          end if
          call FROGG     ('Kinetic Temperature (K)',
     $                    TE(IB), INC, LINE, IZZ, NO)
          call LINER     (1, NO)
          call FROGGY    ('Abundance ratio',
     $                    RABD(IB), INC, LINE, IZZ, NO, IB, KKRAB, N)
          call FROGGY    ('log10(Abundance ratio)',
     $                    RABDL(IB), INC, LINE, IZZ, NO, IB, KKRBL, N)
          call FROGGY    (('Partition Function of '//QLABEL),
     $                   PF(IB), INC, LINE, IZZ, NO, IB, KKPFN, N)
          if(TRIN) then
            call LINER   (1, NO)
            do 104 J = 1,NSL
              write (TIT,103) J
  103         format(6X,'Level',I3,' Radiation Temperature (K)')
              call FROGG (TIT,
     $                    TR(IB,J), INC, LINE, IZZ, NO)
  104       continue
            call FRIG    ('Photoionization Rates Multiplier',
     $                    HJ(IB), INC, LINE, IZZ, NO, IB, JH1, JH2,
     $                    KKPRM, N)
          end if
          if(KSHL) then
            call LINER   (1, NO)
            call FROGG   ('QIN (for K-Shell)',
     $                    CQIN(IB), INC, LINE, IZZ, NO)
          end if
        if(IE.lt.N) goto 102
C
        call LEAFY       (NO, TRIN, IQSFS, JH1, JH2, IZZ)
C
      end if
C     !END
      call BYE ('YODEL')
C
      return
      end

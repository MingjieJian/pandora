      subroutine BOGACH
     $(N1NUP,KAMB,KVLG,HE2SIM,N,NL,XNK,XND,XNKL,XNDL,HEK,HE1,XPBL,KODE)
C
C     Rudolf Loeser, 1989 Sep 22
C---- Supervises final populations update for the diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 HE1, HEK, XND, XNDL, XNK, XNKL, XPBL
      integer IQAN1, KAMB, KION, KODE, KVLG, LUEO, N, N1NUP, NKAR, NL
      logical DOIT, HE2SIM, PRNT
      character MESS*5
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
      equivalence (IQQ(272),IQAN1)
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  PATINA, MOVE1, HALT, MESHED, MASHED, HI, BYE
      intrinsic max
C
C               XNK(N), XND(N,NL), XNDL(N,NL), HEK(N), XNKL(N), HE1(N),
      dimension XNK(*), XND(*),    XNDL(*),    HEK(*), XNKL(*), HE1(*),
C
C               XPBL(Lenpbl)
     $          XPBL(*)
C
      dimension NKAR(3), MESS(3)
C
      data MESS /'H', 'He-I', 'He-II'/
      data NKAR / 1,   4,      5/
C     !EJECT
C
      call HI ('BOGACH')
C     !BEG
      PRNT = KODE.gt.0
      KION = max(KAMB,KVLG)
      DOIT = (KION.gt.0).and.(IQAN1.gt.0)
      if(DOIT) then
        if(N1NUP.eq.1) then
          call MOVE1    (XNKL, N,      XNK)
          call MOVE1    (XNDL, (N*NL), XND)
          if(KION.eq.1) then
C----       Hydrogen
            call PATINA (1, N, NL, XNK, XND, XPBL, 11, 9)
          else if(KION.eq.2) then
C----       Helium-I
            call PATINA (4, N, NL, XNK, XND, XPBL, 14, 12)
          else if(KION.eq.3) then
C----       Helium-II
            call PATINA (5, N, NL, XNK, XND, XPBL, 17, 15)
          else
            write (MSSLIN(1),100) KION
  100       format('KION =',I12,', which is neither 1, 2, nor 3.')
            call HALT   ('BOGACH', 1)
          end if
          if(PRNT) then
            call MESHED ('BOGACH', 3)
            write (LUEO,101) MESS(KION)(:NKAR(KION))
  101       format(' ',A,' populations data file was updated with ',
     $                 'the "Special N1" results.')
          end if
          if(HE2SIM) then
            call PATINA (4, N, 1, HEK, HE1, XPBL, 14, 12)
            if(PRNT) then
              write (LUEO,101) MESS(2)(:NKAR(2))
            end if
          end if
          if(PRNT) then
            write (LUEO,102)
  102       format(' ','NK & ND of the ion-of-the-run were updated ',
     $                 'with the "Special N1" results.')
            call MASHED ('BOGACH')
          end if
        else
          if(PRNT) then
            call MESHED ('BOGACH', 3)
            write (LUEO,103)
  103       format(' ','"Special N1" results were NOT copied into ND ',
     $                 '& NK of the ion-of-the-run, nor into ',
     $                 'population data files.')
            call MASHED ('BOGACH')
          end if
        end if
      end if
C     !END
      call BYE ('BOGACH')
C
      return
      end

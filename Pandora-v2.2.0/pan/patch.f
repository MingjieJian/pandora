      subroutine PATCH
     $(NO,VM,VBMB,PALBET,PBETAL,PBETGM,PGMBET)
C
C     Rudolf Loeser, 1999 Jan 08
C---- Prints diffusion terms calculation data.
C     (This is version 2 of PATCH.)
C     !DASH
      save
C     !DASH
      real*8 PALBET, PBETAL, PBETGM, PGMBET, VBMB, VM
      integer IQAN1, IQHEA, K, KAMB, KVLG, N, NO
      logical VMZERO
      character LAB*5
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C
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
      equivalence (IQQ(281),IQHEA)
C     !DASH
C     !EJECT
      external  PADMA, LINER, MANID, MABEL, BINEL, BELID, BEMAL, ENID,
     $          NAUGHTD, HI, BYE
      intrinsic max
C
C               VBMB(N), PALBET(N), PBETAL(N), PBETGM(N), PGMBET(N),
      dimension VBMB(*), PALBET(*), PBETAL(*), PBETGM(*), PGMBET(*),
C
C               VM(N)
     $          VM(*)
C
      dimension LAB(3)
C
      data LAB /'H    ', 'He-I ', 'He-II'/
C
      call HI ('PATCH')
C     !BEG
      K = max(KAMB,KVLG)
      if((NO.gt.0).and.(K.gt.0)) then
        call PADMA   (NO,'"Diffusion" Calculation')
        write (NO,100) KAMB,KVLG
  100   format(' ',105X,'(KAMB =',I3,', KVLG =',I3,')')
        if(KAMB.gt.0) then
          if(KVLG.gt.0) then
            write (NO,101) LAB(K)
  101       format(' ','Ambipolar diffusion and mass motion ',
     $                 'calculations for ',A)
          else
            write (NO,102) LAB(K)
  102       format(' ','Ambipolar diffusion calculations for ',A)
          end if
        else
          write (NO,103) LAB(K)
  103     format(' ','Mass motion calculations for ',A)
        end if
        call NAUGHTD (VM,1,N,VMZERO)
        call LINER   (1,NO)
        write (NO,104)
  104   format(' ','[* indicates actual parameter values in this run.]')
        call LINER   (1,NO)
        call MABEL   (NO,IQAN1)
        call LINER   (1,NO)
        call BINEL   (NO)
        call LINER   (1,NO)
        call BELID   (NO)
        call LINER   (1,NO)
        call BEMAL   (NO,KVLG)
        call LINER   (1,NO)
        call MANID   (NO,IQHEA)
        call LINER   (1,NO)
        call ENID    (NO,N,IQAN1,KAMB,VM,VBMB,PALBET,PBETAL,PGMBET,
     $                PBETGM)
      end if
C     !END
      call BYE ('PATCH')
C
      return
      end

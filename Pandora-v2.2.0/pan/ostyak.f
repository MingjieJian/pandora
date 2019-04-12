      subroutine OSTYAK
     $(Z,N,NTAN,MSKIP,NSHL,NRPMX,FRR,MRR,R1N,XSHL,EMSHL,CSHL,WSHL,
     $ CODSRW,XDSK,EMDSK,CDSK,WDSK,LFLX)
C
C     Rudolf Loeser, 1981 Oct 27
C---- Prints, for NUBA.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CODSRW, CSHL, EMDSK, EMSHL, FRR, R1N, WDSK, WSHL,
     $       XDSK, XSHL, Z
      integer I, LFLX, LU, MRR, MSKIP, N, NRPMX, NSHL, NTAN
      logical PRNTZ
      character BLANK*1, LAB*9
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external PILLAR, PRIAM, LINER, DEZA, OSSET, OMAR, HI, BYE
C
C               FRR(MRR), XSHL(NSHL,NRPMX), EMSHL(N,NSHL), XDSK(N,MRR),
      dimension FRR(*),   XSHL(*),          EMSHL(*),      XDSK(*),
C
C               CSHL(N,NSHL), WSHL(N,NSHL), CODSRW(NSHL), EMDSK(N,MRR),
     $          CSHL(*),      WSHL(*),      CODSRW(*),    EMDSK(*),
C
C               CDSK(N,MRR), WDSK(N,MRR), Z(N)
     $          CDSK(*),     WDSK(*),     Z(*)
C
      dimension LAB(3)
C
      data LAB   /'; for WN.', '; for WH.', '.        '/
      data PRNTZ /.false./
C     !EJECT
C
      call HI ('OSTYAK')
C     !BEG
      call PILLAR    (LU)
      if(LU.gt.0) then
        call PRIAM   (LU, 'SPHERE', 6)
        call LINER   (2, LU)
        write (LU,100) R1N
  100   format(' ','R1N (km)',2X,1PE13.5)
        call LINER   (1, LU)
        write (LU,101) (Z(I),I=1,N)
  101   format(' ','Z (km)',4X,1P9E13.5/
     $        (' ',10X,9E13.5))
        call LINER   (1, LU)
        write (LU,102) NTAN,NSHL,NRPMX
  102   format(' ','NTAN',6X,I13,94X,'(',I3,',',I4,')')
        if(NTAN.eq.1) then
          call LINER (1, LU)
          write (LU,103) MSKIP
  103     format(' ','MSKIP',5X,I13)
        end if
        if(MRR.gt.0) then
          call LINER (1, LU)
          write (LU,104) (FRR(I),I=1,MRR)
  104     format(' ','FRR',7X,1P9E13.5/(' ',10X,9E13.5))
        end if
        call LINER   (2, LU)
        call DEZA    (LU, NRPMX, NSHL, XSHL, CODSRW)
        call LINER   (2, LU)
        write (LU,105)
  105   format(' ','Values of cos(theta) along Shell Rays.')
        call OSSET   (LU, N,NSHL, EMSHL)
        if(LFLX.gt.0) then
          call LINER (2, LU)
          write (LU,106) LAB(1)
          call OSSET (LU, N, NSHL, CSHL)
          call LINER (2, LU)
          write (LU,106) LAB(2)
          call OSSET (LU, N, NSHL, WSHL)
        else
          call LINER (2, LU)
          write (LU,106) LAB(3)
  106     format(' ','Angle integration weights along Shell Rays',A9)
          call OSSET (LU, N, NSHL, CSHL)
        end if
C     !EJECT
        call LINER   (2, LU)
        write (LU,107)
  107   format(' ','Distances along Disk Rays (cm).')
        call OMAR    (LU, N, MRR, XDSK, BLANK, PRNTZ)
        call LINER   (2, LU)
        write (LU,108)
  108   format(' ','Values of cos(theta) along Disk Rays.')
        call OMAR    (LU, N, MRR, EMDSK, BLANK, PRNTZ)
        if(LFLX.gt.0) then
          call LINER (2, LU)
          write (LU,109) LAB(1)
          call OMAR  (LU, N, MRR, CDSK, BLANK, PRNTZ)
          call LINER (2, LU)
          write (LU,109) LAB(2)
          call OMAR  (LU, N, MRR, WDSK, BLANK, PRNTZ)
        else
          call LINER (2, LU)
          write (LU,109) LAB(3)
  109     format(' ','Angle integration weights along Disk Rays',A9)
          call OMAR  (LU, N, MRR, CDSK, BLANK, PRNTZ)
        end if
      end if
C     !END
      call BYE ('OSTYAK')
C
      return
      end

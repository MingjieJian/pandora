      subroutine TURNIP
     $(LU,LJ,IW,N,K,DL,XJNU,XIK,SLF,VXI,XQSF,XRD,ZRD,YRD,S,GTN,PHI)
C
C     Rudolf Loeser, 2005 Jan 14
C---- Prints for CATANA.
C     (This is version 5 of TURNIP.)
C     !DASH
      save
C     !DASH
      real*8 DL, GTN, PHI, S, SLF, VXI, XIK, XJNU, XKL, XQSF, XRD, YRD,
     $       ZRD, dummy
      integer I, IPRDD, IW, J, K, LJ, LU, N
      logical PRINT
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
      equivalence (KZQ( 98),IPRDD)
C     !DASH
C     !EJECT
      external VITIM, LINER, FULDA, HI, BYE
C
      dimension IW(*)
C
C               XIK(N,K), VXI(N,K), SLF(N,K), XRD(N,K), YRD(N,K), S(N),
      dimension XIK(N,*), VXI(N,*), SLF(N,*), XRD(N,*), YRD(N,*), S(*),
C
C               XQSF(N,K), XJNU(N,K), PHI(N,K), GTN(N), DL(K), ZRD(N,K)
     $          XQSF(N,*), XJNU(*),   PHI(N,*), GTN(*), DL(*), ZRD(N,*)
C
      call HI ('TURNIP')
C     !BEG
      if(LU.gt.0) then
        call LINER     (2, LU)
        do 103 J = 1,K
          call VITIM   (LU, 0, K, J, DL, dummy, dummy, PRINT)
          if(PRINT) then
            call LINER (1, LU)
            write (LU,100)
  100       format(' ',16X,'X',11X,'KL',10X,'VXI',10X,'SLF',10X,'QSF',
     $                 12X,'S',10X,'XRD',10X,'ZRD',10X,'YRD')
            call LINER (1, LU)
            do 102 I = 1,N,IPRDD
              XKL = PHI(I,J)*GTN(I)
              write (LU,101) I,XIK(I,J),XKL,VXI(I,J),SLF(I,J),
     $                         XQSF(I,J),S(I),XRD(I,J),ZRD(I,J),
     $                         YRD(I,J)
  101         format(' ',I4,1P9E13.5)
  102       continue
          end if
  103   continue
      end if
C
      if(LJ.gt.0) then
C----   Print Jnu
        call FULDA     (LJ, N, K, XJNU, DL, 'Jnu', IW)
      end if
C     !END
      call BYE ('TURNIP')
C
      return
      end

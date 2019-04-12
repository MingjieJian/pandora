      subroutine BATAK
     $(LU,LJ,IW,N,K,DL,XJNU,XIK,FJR,RXI,FRD,GRD,XRD,ZRD,YRD,CORE,GMMA,
     $ XC,XP,XR)
C
C     Rudolf Loeser, 2005 Jan 19
C---- Prints for DELOS.
C     (This is version 2 of BATAK.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DL, FJR, FRD, GMMA, GRD, RXI, XC, XIK, XJNU, XP, XR,
     $       XRD, YRD, ZRD, dummy
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
      external OSCAR, VITIM, LINER, FULDA, HI, BYE
C
      dimension IW(*)
C
C               XJNU(N,K), FJR(N,K), XRD(N,K), FRD(N,K), GRD(N,K),
      dimension XJNU(N,*), FJR(N,*), XRD(N,*), FRD(N,*), GRD(N,*),
C
C               XIK(N,K), RXI(N,K), DL(K), YRD(N,K), ZRD(N,K)
     $          XIK(N,*), RXI(N,*), DL(*), YRD(N,*), ZRD(N,*)
C
      call HI ('BATAK')
C     !BEG
      if(LU.gt.0) then
        do 103 J = 1,K
          call VITIM   (LU, 0, K, J, DL, dummy, dummy, PRINT)
          if(PRINT) then
            call LINER (1, LU)
            write (LU,100)
  100       format(' ',4X,12X,'X',10X,'FJR',10X,'RXI',10X,'FRD',
     $                 10X,'GRD',10X,'XRD',4X,'ZRD=1-XRD',10X,'YRD')
            call LINER (1, LU)
            do 102 I = 1,N,IPRDD
              write (LU,101) I,XIK(I,J),FJR(I,J),RXI(I,J),FRD(I,J),
     $                         GRD(I,J),XRD(I,J),ZRD(I,J),YRD(I,J)
  101         format(' ',I4,1P8E13.5)
  102       continue
          end if
  103   continue
      end if
C
      if(LJ.gt.0) then
        if(LU.le.0) then
          call OSCAR   (LJ, 2, 0, CORE, GMMA, XC, XP, XR)
        end if
        call FULDA     (LJ, N, K, XJNU, DL, 'Jnu', IW)
      end if
C     !END
      call BYE ('BATAK')
C
      return
      end

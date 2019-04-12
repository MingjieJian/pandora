      subroutine GALLA
     $(LU,N,K,DL,XJNU,VXI,XQSF,XRD,YRD,S,SNU,SLR)
C
C     Rudolf Loeser, 2005 Jan 19
C---- Prints for CIMON.
C     (This is version 4 of GALLA.)
C     !DASH
      save
C     !DASH
      real*8 DL, S, SLR, SNU, VXI, XJNU, XQSF, XRD, YRD, dummy
      integer I, IPRDD, J, K, LU, N
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
      external VITIM, LINER, HI, BYE
C
C               XRD(N,K), VXI(N,K), S(N), SNU(N,K), SLR(N,K), YRD(N,K),
      dimension XRD(N,*), VXI(N,*), S(*), SNU(N,*), SLR(N,*), YRD(N,*),
C
C               XJNU(N,K), XQSF(N,K), DL(K)
     $          XJNU(N,*), XQSF(N,*), DL(*)
C
      call HI ('GALLA')
C     !BEG
      if(LU.gt.0) then
        do 103 J = 1,K
          call VITIM   (LU, 0, K, J, DL, dummy, dummy, PRINT)
          if(PRINT) then
            call LINER (1, LU)
            write (LU,100)
  100       format(' ',4X,10X,'JNU',10X,'VXI',10X,'QSF',10X,'SNU',
     $                 12X,'S',10X,'XRD',10X,'YRD',10X,'SLR')
            call LINER (1, LU)
            do 102 I = 1,N,IPRDD
              write (LU,101) I,XJNU(I,J),VXI(I,J),XQSF(I,J),SNU(I,J),
     $                         S(I),XRD(I,J),YRD(I,J),SLR(I,J)
  101         format(' ',I4,1P8E13.5)
  102       continue
          end if
  103   continue
      end if
C     !END
      call BYE ('GALLA')
C
      return
      end

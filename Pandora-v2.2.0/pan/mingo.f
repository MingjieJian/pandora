      subroutine MINGO
     $(KODE,I,J,W,B)
C
C     Rudolf Loeser, 1984 Jan 24
C---- Saves fudging data, for later printing.
C     (This is version 3 of MINGO.)
C     !DASH
      save
C     !DASH
      real*8 B, W
      integer I, IOVER, ITER, J, KODE
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
C
C---- SELGI       as of 1999 Sep 13
      integer     KNFMX
      parameter   (KNFMX=50)
C     (Remember to recompile all users when changing KNFMX)
      integer     KNTF,INF1,INF2,INF3,INF4
      real*8      FUJJ,FVAL
      dimension   INF1(KNFMX),INF2(KNFMX),INF3(KNFMX),INF4(KNFMX),
     $            FUJJ(KNFMX),FVAL(KNFMX)
      common      /SELGI1/ KNTF,INF1,INF2,INF3,INF4
      common      /SELGI2/ FUJJ,FVAL
C     Saves B-ratios computation fudging data, for later printing.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('MINGO')
C     !BEG
      if(KODE.eq.1) then
        if(KNTF.lt.KNFMX) then
          KNTF = KNTF+1
          INF1(KNTF) = IOVER
          INF2(KNTF) = ITER
          INF3(KNTF) = I
          INF4(KNTF) = J
          FUJJ(KNTF) = W
          FVAL(KNTF) = B
        end if
      end if
C     !END
      call BYE ('MINGO')
C
      return
      end

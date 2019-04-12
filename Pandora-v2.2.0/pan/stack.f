      subroutine STACK
     $(LU,MN1,N,Z,TE,ZT,KZAS,NZAS,KZUNL)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Prints iterative analysis of KZAUG/KZUNL, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 TE, Z, ZT
      integer IOMX, IOVER, KSUM, KZAS, KZUNL, LU, MN1, N, NU, NZAS
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
      equivalence (KZQ(  8),IOMX )
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
      equivalence (LEST( 2),IOVER)
C     !DASH
      external  IRRSUM, ABJECT, SATRAP, HI, BYE
      intrinsic min
C
C               JTMX = ITN1R*ITKZA+1
C
C               KZUNL(N,20), KZAS(N,JTMX), ZT(N), TE(N), Z(N)
      dimension KZUNL(N,*),  KZAS(N,*),    ZT(*), TE(*), Z(*)
C     !EJECT
C
      call HI ('STACK')
C     !BEG
      if(LU.gt.0) then
        call IRRSUM     (KZAS(1,NZAS), MN1, KSUM)
        if(KSUM.gt.0) then
C
          if(NZAS.gt.0) then
            call ABJECT (LU)
            write (LU,100)
  100       format(' ','Successive changes in the set of ',
     $                 'Z augmentation counters.')
            call SATRAP (LU, MN1, N, Z, TE, ZT, KZAS, NZAS, 1)
          end if
C
          if(IOVER.eq.IOMX) then
            call ABJECT (LU)
            write (LU,101)
  101       format(' ','Successive changes in the set of "unlimited" ',
     $                 'Z augmentation counters.')
            NU = min(IOVER,20)
            call SATRAP (LU, MN1, N, Z, TE, ZT, KZUNL, NU, 0)
          end if
C
        end if
      end if
C     !END
      call BYE ('STACK')
C
      return
      end

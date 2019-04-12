      subroutine KASTEN
     $(N,KZAUG,KZAS,NZAS,KZUNL)
C
C     Rudolf Loeser, 2004 Feb 03
C---- Initializes KZAS and KZUNL, for CARAMBA.
C     !DASH
      save
C     !DASH
      integer IOVER, KZAS, KZAUG, KZUNL, N, NZAS
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
C     !DASH
      external MOVEI, ZEROI, HI, BYE
C
C               JTMX = ITN1R*ITKZA+1
C
C               KZAUG(N), KZAS(N,JTMX), KZUNL(N,20)
      dimension KZAUG(*), KZAS(N,*),    KZUNL(N,*)
C
      call HI ('KASTEN')
C     !BEG
      NZAS = 1
      call MOVEI   (KZAUG, 1, N, KZAS(1,NZAS), 1, N)
C
      if(IOVER.eq.1) then
        call ZEROI (KZUNL(1,1), 1, N)
      else if(IOVER.le.20) then
        call MOVEI (KZUNL(1,(IOVER-1)), 1, N, KZUNL(1,IOVER), 1, N)
      end if
C     !END
      call BYE ('KASTEN')
C
      return
      end

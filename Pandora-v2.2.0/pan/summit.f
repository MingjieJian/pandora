      subroutine SUMMIT
     $(M,SA,CHKA,VEC,IPNT,ITER,MN1,N,KZAUG,KZANX,KZAS,NZAS,KZUNL,
     $ CHANGE,DUMP)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Updates the Z-augmentation counters, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 CHKA, CN1S, RCHK, SA, VEC
      integer I, IOVER, IPNT, ITER, J, JJ, KSUM, KZANX, KZAS, KZAUG,
     $        KZUNL, M, MN1, MXTAP, N, NZAS
      logical BANG, CHANGE, DUMP, HANG, KILROY, LILROY, UPUNL
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
      equivalence (KZQ(201),MXTAP)
      equivalence (RZQ(176),CN1S )
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
      external IRRSUM, MUTISM, SITTER, CUBBIE, TUTTUT, MASHED, MOVEI,
     $         HI, BYE
C
C               J = N+MXTAP             JTMX = ITN1R*ITKZA+1
C
C               CHKA(J), KZAUG(N), KZANX(N), KZAS(N,JTMX), KZUNL(N,20),
      dimension CHKA(*), KZAUG(*), KZANX(*), KZAS(N,*),    KZUNL(N,*),
C
C               SA(J), VEC(J), IPNT(J)
     $          SA(*), VEC(*), IPNT(*)
C     !EJECT
C
      call HI ('SUMMIT')
C     !BEG
      CHANGE = .false.
      UPUNL  = IOVER.le.20
      LILROY = .true.
C
      call IRRSUM         (KZAUG, MN1, KSUM)
      if(KSUM.lt.MXTAP) then
        KILROY = .true.
C
        do 100 JJ = 1,M
          BANG = .false.
          HANG = .false.
          call MUTISM     (SA, CHKA, M, VEC, IPNT, LILROY, JJ, J, RCHK,
     $                     DUMP)
          if(RCHK.gt.CN1S) then
            call SITTER   (J, KZANX, MN1, I)
            call CUBBIE   (I,     UPUNL, KZUNL(1,IOVER), KZAUG, KSUM,
     $                     CHANGE, BANG)
            if((KZANX(I).eq.J).and.(I.gt.1)) then
              call CUBBIE ((I-1), UPUNL, KZUNL(1,IOVER), KZAUG, KSUM,
     $                     CHANGE, HANG)
            end if
            if(DUMP) then
              call TUTTUT (J, CHKA(J), SA(J), RCHK, CN1S, I, KZAUG(I),
     $                     BANG, KZAUG(I-1), HANG, KSUM, KILROY,
     $                     'SUMMIT')
            end if
            if(KSUM.eq.MXTAP) then
              goto 101
            end if
          end if
  100   continue
C
  101   continue
        if(CHANGE) then
          NZAS = NZAS+1
          call MOVEI      (KZAUG, 1, MN1, KZAS(1,NZAS), 1, MN1)
        end if
        if(.not.KILROY) then
          call MASHED     ('SUMMIT')
        end if
C
      end if
C     !END
      call BYE ('SUMMIT')
C
      return
      end

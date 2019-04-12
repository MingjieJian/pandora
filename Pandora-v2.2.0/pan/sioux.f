      subroutine SIOUX
     $(N,CSF,B,BTR,BC)
C
C     Rudolf Loeser, 2006 Feb 23
C---- Computes BC at line-center.
C     (This is version 5 of SIOUX.)
C     !DASH
      save
C     !DASH
      real*8 B, BC, BTR, CSF
      integer IOVER, ITER, MOMET, N
      logical DOIT
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
      equivalence (LEST(17),MOMET)
C     !DASH
      external PUCCOON, HI, BYE
C
C               CSF(N), B(N), BTR(N), BC(N)
      dimension CSF(*), B(*), BTR(*), BC(*)
C
      call HI ('SIOUX')
C     !BEG
      DOIT = .false.
      if(MOMET.gt.0) then
        if(ITER.eq.1) then
          DOIT = .true.
        end if
      else
        if((IOVER*ITER).eq.1) then
          DOIT = .true.
        end if
      end if
C
      if(DOIT) then
        call PUCCOON (N, CSF, B, BTR, BC)
      end if
C     !END
      call BYE ('SIOUX')
C
      return
      end

      subroutine PALI
     $(N,NSL,R,KODE)
C
C     Rudolf Loeser, 1989 Aug 21
C---- Checks and adjusts rates R, for HOGRE.
C     !DASH
      save
C     !DASH
      real*8 R
      integer N, NNSL, NSL
      logical KODE, RZERO
C     !DASH
      external NAUGHTD, ZERO1, HI, BYE
C
C               R(N,NSL)
      dimension R(*)
C
      call HI ('PALI')
C     !BEG
      KODE = .false.
C
      NNSL = N*NSL
      if(NNSL.gt.0) then
        call NAUGHTD (R,1,NNSL,RZERO)
        if(.not.RZERO) then
          call ZERO1 (R,NNSL)
          KODE = .true.
        end if
      end if
C     !END
      call BYE ('PALI')
C
      return
      end

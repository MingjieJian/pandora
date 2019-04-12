      subroutine PLAUTUS
     $(N,FZLIM,DZMSS,ZOLD,ZNEW,ZED,ZQ)
C
C     Rudolf Loeser, 1985 Feb 06
C---- Controls editing of new Z-scale, for CYRILLO.
C     !DASH
      save
C     !DASH
      real*8 DZMSS, FZLIM, ZED, ZNEW, ZOLD, ZQ
      integer I, N
C     !DASH
      external BOOK, ROOK, HI, BYE
C
C               ZOLD(N), ZNEW(N), ZED(N)
      dimension ZOLD(*), ZNEW(*), ZED(*)
C
      call HI ('PLAUTUS')
C     !BEG
      call BOOK   (ZOLD(1),ZOLD(N),DZMSS,ZQ)
      do 100 I = 1,N
        call ROOK ((ZOLD(I)-ZQ),(ZNEW(I)-ZQ),FZLIM,ZED(I))
C
        ZED(I) = ZED(I)+ZQ
  100 continue
C     !END
      call BYE ('PLAUTUS')
C
      return
      end

      subroutine DESERT
     $(N,RNDT,F1,EP2,EP1,XLP,D,ERT)
C
C     Rudolf Loeser, 1974 Dec 06
C---- Sets up ERT, for Lyman.
C     (This is version 2 of DESERT.)
C     !DASH
      save
C     !DASH
      real*8 D, EP1, EP2, ERT, F1, RNDT, XDEN, XLP, XNUM
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               EP2(N), EP1(N), RNDT(N), F1(N), ERT(N), XLP(N), D(N)
      dimension EP2(*), EP1(*), RNDT(*), F1(*), ERT(*), XLP(*), D(*)
C
      call HI ('DESERT')
C     !BEG
      do 100 I = 1,N
        XNUM = ((EP2(I)+RNDT(I))*F1(I)+XLP(I))
        XDEN = EP1(I)+D(I)
        call DIVIDE (XNUM, XDEN, ERT(I))
  100 continue
C     !END
      call BYE ('DESERT')
C
      return
      end

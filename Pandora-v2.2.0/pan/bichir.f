      subroutine BICHIR
     $(TAUUL,TRA,KOUNT)
C
C     Rudolf Loeser, 1986 Oct 02
C---- Sets up "Artificial TAU" for RHO/W calculation.
C     !DASH
      save
C     !DASH
      real*8 TAUUL, TRA
      integer I, KOUNT, KTRAS, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LEST(38),KTRAS)
C     !DASH
      external  MOVE1, HI, BYE
      intrinsic max
C
C               TAUUL(N), TRA(N)
      dimension TAUUL(*), TRA(*)
C
      call HI ('BICHIR')
C     !BEG
      if(KOUNT.eq.1) then
        call MOVE1 (TAUUL,N,TRA)
        KTRAS = 1
      else
C
        do 100 I = 2,N
          TRA(I) = max(TRA(I),TAUUL(I))
  100   continue
        KTRAS = KTRAS+1
C
      end if
C     !END
      call BYE ('BICHIR')
C
      return
      end

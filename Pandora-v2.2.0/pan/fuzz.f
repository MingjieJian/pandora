      subroutine FUZZ
     $(X,W,Z,TAU5000,OP5000,T5,NTITER,CONVERG,IMG)
C
C     Rudolf Loeser, 1980 Jul 21
C---- Computes new TAU5000 for H.S.E., and checks for convergence.
C     (This is version 2 of FUZZ.)
C     !DASH
      save
C     !DASH
      real*8 OP5000, T5, TAU5000, W, X, Z
      integer IMG, KZERO, N, NTITER, jummy
      logical CONVERG, lummy1, lummy2
      character LABEL*100
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
      equivalence (LEST(10),KZERO)
C     !DASH
      external DISMAL, MONITOR, LOTUS, CAUTION, HI, BYE
C
      dimension X(*), W(*)
C
C               T5(12), OP5000(N), TAU5000(N), Z(N), IMG(N)
      dimension T5(*),  OP5000(*), TAU5000(*), Z(*), IMG(*)
C
      data LABEL /'TAU5000'/
C     !EJECT
C
      call HI ('FUZZ')
C     !BEG
C---- Compute TAU5000
      call DISMAL      (X,W,1,N,OP5000,TAU5000,LABEL,jummy,lummy1,
     $                  lummy2,IMG)
C
      if(NTITER.gt.0) then
        if(NTITER.eq.1) then
C----     Find adjustment index
          call MONITOR (N,Z,KZERO)
        end if
C----   Get TAU5000 at Z=0
        call LOTUS     (N,Z,TAU5000,KZERO,T5(NTITER))
C----   Check for convergence, and set signal
        call CAUTION   (T5,NTITER,CONVERG)
      end if
C     !END
      call BYE ('FUZZ')
C
      return
      end

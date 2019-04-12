      subroutine SING1
     $(N,FLAG,PNT,KODE,KRET)
C     Rudolf Loeser, 1979 Apr 05
C---- Common initialization code of "SING" routines.
C     !DASH
      save
C     !DASH
      real*4 XNC, XNR, ZERO
      integer FLAG, I, IL, IN, IU, J, K, KODE, KRET, L, M, N, NC, NR,
     $        PNT
      logical DOPT
C     !COM
      common  /SING01/ I,J,K,L,M,NC,NR,IU,IL
      common  /SORTST/ XNC,XNR
C     !DASH
      dimension PNT(*), IU(23), IL(23)
C
      data ZERO /0./
C
C     !BEG
      KRET = 1
      DOPT = KODE.eq.1
      if(N.lt.1) then
        KRET = 2
        FLAG = 0
      else if(N.eq.1) then
        XNC  = ZERO
        XNR  = ZERO
        KRET = 2
        FLAG = 1
        if(DOPT) then
          PNT(1) = 1
        end if
      else
        NC = 0
        NR = 0
        M  = 1
        I  = 1
        J  = N
        if(DOPT) then
          do 100 IN = 1,N
            PNT(IN) = IN
  100     continue
        end if
      end if
C     !END
C
      return
      end

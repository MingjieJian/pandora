      subroutine CALTROP
     $(CONT,NOPAC,CONSWI,MXCHR)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Encodes contribution switches, for SHARI,
C     (This is version 2 of CALTROP.)
C     !DASH
      save
C     !DASH
      real*8 CONT
      integer I, J, K, KONT, L, M, MXCHR, NOPAC
      character CONSWI*(*)
C     !DASH
      external HI, BYE
C
C               CONT(NOPAC)
      dimension CONT(*)
C     !EJECT
C
      call HI ('CALTROP')
C     !BEG
      CONSWI = ' '
      K = 0
      L = 0
C
      do 102 I = 1,6
C
        do 101 M = 1,2
C
          do 100 J = 1,5
C
            L = L+1
            if(L.gt.NOPAC) goto 103
            K = K+1
            if(K.gt.MXCHR) goto 103
C
            KONT = CONT(L)
            if(KONT.eq.0) then
                                    CONSWI(K:K) = '-'
            else if(KONT.eq.1) then
                                    CONSWI(K:K) = 'X'
            else if(KONT.eq.2) then
                                    CONSWI(K:K) = 'z'
            else
                                    CONSWI(K:K) = '.'
            end if
C
  100     continue
          K = K+1
          if(K.gt.MXCHR) goto 103
C
  101   continue
        K = K+1
        if(K.gt.MXCHR) goto 103
C
  102 continue
C
  103 continue
C     !END
      call BYE ('CALTROP')
C
      return
      end

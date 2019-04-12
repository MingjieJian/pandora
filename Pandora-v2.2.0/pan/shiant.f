      subroutine SHIANT
     $(IMAGE,NLIN,NCOL,TE,IBEG,IEND)
C
C     Rudolf Loeser, 1996 Mar 06
C---- Enters temperature values into plot, for SKYE.
C     !DASH
      save
C     !DASH
      real*8 T, TE, THI, TLO, X
      integer I, IBEG, IEND, IT, J, JX, K, NCOL, NLIN
      character BLANK*1, IMAGE*(*), VALUE*11
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external KWHERE, KPLOTP, HI, BYE
C
C               TE(N)
      dimension TE(*)
C
      data TLO,THI /0.D0, 1.D10/
C
      call HI ('SHIANT')
C     !BEG
      do 102 I = IBEG,IEND
        T = TE(I)
        if((T.gt.TLO).and.(T.lt.THI)) then
C
          write (VALUE,100) T
  100     format(F11.0)
          X = I
          call KWHERE     (IMAGE,X,T,JX,IT)
          K = NLIN-11
          do 101 J = 1,10
            K = K+1
            if(VALUE(J:J).ne.BLANK) then
              call KPLOTP (IMAGE,JX,K,VALUE(J:J))
            end if
  101     continue
C
        end if
  102 continue
C     !END
      call BYE ('SHIANT')
C
      return
      end

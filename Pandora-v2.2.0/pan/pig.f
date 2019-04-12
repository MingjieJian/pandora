      subroutine PIG
     $(NO,IS,IE,N,NL,CIJ,PIJ)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Does RIJ, at depths IS through IE.
C     (This is version 2 of PIG.)
C     !DASH
      save
C     !DASH
      real*8 C, CIJ, P, PIJ, R
      integer I, IE, IN, IS, J, JS, KNT, N, NL, NO
      logical HEAD
C     !DASH
      external  INDXIJ, MOVE1, LINER, RIG, HI, BYE
C
C               CIJ(N,NL**2), PIJ(N,NL**2)
      dimension CIJ(N,*),     PIJ(N,*)
C
      dimension C(11), P(11), R(11)
C
      call HI ('PIG')
C     !BEG
      KNT  = IE-IS+1
      HEAD = .true.
      do 102 I = 1,NL
C
        do 101 J = 1,NL
C
          if(I.ne.J) then
            call INDXIJ  (I,J, JS)
            call MOVE1   (CIJ(IS,JS),KNT,C)
            call MOVE1   (PIJ(IS,JS),KNT,P)
            if(HEAD) then
              call LINER (2,NO)
              write (NO,100) (IN,IN=IS,IE)
  100         format(' ','Trans',2X,'Depth',I6,10I11)
              HEAD = .false.
            end if
            call RIG     (NO,I,J,C,P,R,KNT)
          end if
C
  101   continue
C
  102 continue
C     !END
      call BYE ('PIG')
C
      return
      end

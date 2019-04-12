      subroutine ONION
     $(K,L,DL,YNT,IMAGE,PSYM,KOSY)
C
C     Rudolf Loeser, 1978 May 01
C---- Enters points into an emission profile graph.
C     If KOSY=1, then ONION uses the plotting symbols 'A', 'B', 'C',
C     etc., to plot various profiles; if KOSY=2, then it uses 'PSYM'.
C     !DASH
      save
C     !DASH
      real*8 DL, Y, YNT, ZERO
      integer I, J, K, KOSY, L, LINC
      character IMAGE*(*), KSYM*1, PSYM*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external BEIGE, HI, BYE
C
C               DL(K), YNT(K,L)
      dimension DL(*), YNT(K,*)
C
      call HI ('ONION')
C     !BEG
      if(KOSY.eq.2) then
        KSYM = PSYM
      end if
C
      do 101 J = 1,L
        if(KOSY.eq.1) then
          KSYM = SYMBS(J)
        end if
        LINC = 1
        do 100 I = 1,K
          if(YNT(I,J).gt.ZERO) then
            Y = log10(YNT(I,J))
            call BEIGE  (IMAGE, DL(I), Y, KSYM, LINC)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('ONION')
C
      return
      end

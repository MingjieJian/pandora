      subroutine FLOTE
     $(N,B1,KK,XK,XNUK,EMUX,SLY)
C
C     Rudolf Loeser, 1974 Dec 30
C---- Computes SLY, for ROPE.
C     !DASH
      save
C     !DASH
      real*8 B1, CA, CON7, EMUX, ONE, SLY, XDEN, XK, XNUK, XNUM, ZERO
      integer I, J, KK, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, RIGEL, HI, BYE
C
C               B1(N), XK(KKX), EMUX(N,KKX), SLY(N,KKX)
      dimension B1(*), XK(*),   EMUX(N,*),   SLY(N,*)
C
      call HI ('FLOTE')
C     !BEG
      call RIGEL        (7, CON7)
      CA = CON7*(XNUK**3)
C
      do 101 J = 1,KK
C
        XNUM = CA*(XK(J)**3)
        do 100 I = 1,N
          if(EMUX(I,J).eq.ZERO) then
            SLY(I,J) = ZERO
          else
            XDEN = B1(I)/EMUX(I,J)-ONE
            call DIVIDE (XNUM, XDEN, SLY(I,J))
          end if
  100   continue
C
  101 continue
C     !END
      call BYE ('FLOTE')
C
      return
      end

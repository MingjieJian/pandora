      subroutine MURIEL
     $(N,IU,IL,XNU,SET,S)
C
C     Rudolf Loeser, 2004 May 19
C---- Computes S directly.
C
C     This takes the place of "S-from-number-densities."
C
C     (This is version 7 of MURIEL.)
C     !DASH
      save
C     !DASH
      real*8 CON7, DNU, ONE, RSET, S, SET, XDEN, XNU, XNUM
      integer I, IL, IU, IUL, N
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, INDXUL, DIVIDE, HI, BYE
C
C               XNU(NSL), S(N), SET(N,MUL)
      dimension XNU(*),   S(*), SET(N,*)
C
      data KILROY,CON7 /.true., 0.D0/
C
      call HI ('MURIEL')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL  (7, CON7)
      end if
C
      call INDXUL   (IU, IL, IUL)
      DNU  = XNU(IU)-XNU(IL)
      XNUM = CON7*(DNU**3)
C
      do 100 I = 1,N
        call DIVIDE (ONE, SET(I,IUL), RSET)
        XDEN = RSET-ONE
        call DIVIDE (XNUM, XDEN, S(I))
  100 continue
C     !END
      call BYE ('MURIEL')
C
      return
      end

      subroutine RANGOON
     $(XLM)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Exhibits behavior of calculated H-bf Gaunt factor.
C     !DASH
      save
C     !DASH
      real*8 G, XINC, XLM, ZERO
      integer J, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external GUNNAR, LINER, HI, BYE
C
      dimension  XINC(9), G(17,9)
C
      data XINC /-1.D1,  -1.D0,  -1.D-1, -1.D-2, 0.D0,
     $            1.D-2,  1.D-1,  1.D0,   1.D1/
C
      call HI ('RANGOON')
C     !BEG
      if(XLM.ne.ZERO) then
        do 101 N = 0,16
          do 100 J = 1,9
            call GUNNAR (N, (XLM+XINC(J)), G(N+1,J))
  100     continue
  101   continue
C
        call LINER      (1, LUEO)
        write (LUEO,102) XLM,(XINC(J),J=1,9)
  102   format(' ','H-bf Gaunt factor fits near XLM =',1PE20.13,
     $             ', using XLM-increments of'//
     $         ' ',10X,1P9E13.5)
        call LINER      (1, LUEO)
        write (LUEO,103) (N,(G(N+1,J),J=1,9),N=0,16)
  103   format(' ','n =',I3,4X,1P9E13.5)
      end if
C     !END
      call BYE ('RANGOON')
C
      return
      end

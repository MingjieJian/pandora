      subroutine TBAR
     $(A,Z,N,AA,ZZ,DOOZ)
C
C     Rudolf Loeser, 2003 Apr 29
C---- Encodes print lines, for IBAR.
C     !DASH
      save
C     !DASH
      real*8 A, Z
      integer I, KNT, N
      logical DOOZ
      character AA*13, BLANK*1, ZZ*13
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external SETC, HI, BYE
C
      dimension A(9), Z(9), AA(9), ZZ(9)
C
      call HI ('TBAR')
C     !BEG
      call SETC (AA,1,9,BLANK)
      call SETC (ZZ,1,9,BLANK)
      KNT = 0
      do 102 I = 1,N
        write (AA(I),100) A(I)
  100   format(1PE13.5)
        if(A(I).ne.Z(I)) then
          KNT = KNT+1
          write (ZZ(I),101) Z(I)
  101     format('(',1PE11.4,')')
        end if
  102 continue
      DOOZ = KNT.gt.0
C     !END
      call BYE ('TBAR')
C
      return
      end

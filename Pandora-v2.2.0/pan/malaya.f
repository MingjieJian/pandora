      subroutine MALAYA
     $(XI,NL,I,J,XO)
C
C     Rudolf Loeser, 1968 Jun 11
C---- MALAYA forms the output matrix XO by deleting row I and column J
C     from the input matrix XI.
C     !DASH
      save
C     !DASH
      real*8 XI, XO
      integer I, II, IO, J, JI, JO, NL
C     !DASH
      external HI, BYE
C
C               XI(NL,NL), XO((NL-1),(NL-1))
      dimension XI(NL,*),  XO((NL-1),*)
C
      call HI ('MALAYA')
C     !BEG
      JO = 0
      do 101 JI = 1,NL
        if(JI.ne.J) then
          JO = JO+1
C
          IO = 0
          do 100 II = 1,NL
            if(II.ne.I) then
              IO = IO+1
C
              XO(IO,JO) = XI(II,JI)
C
            end if
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('MALAYA')
C
      return
      end

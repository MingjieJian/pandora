      subroutine PUN
     $(A,IS,IE,INC,WRAP,LU)
C
C     Rudolf Loeser, 1981 Jun 02
C---- Writes the values of A, for PUNT.
C     (See also PUD.)
C     (This is version 2 of PUN.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, INC, IS, J, LU
      logical KILROY, WRAP
      character BLANK*1, CLOSE*16, F*16
C     !DASH
      external  PPUN, HI, BYE
C
C               A(INC,m)
      dimension A(*)
C
      dimension F(5)
C
      data CLOSE,BLANK /' ) >            ', ' '/
C
      call HI ('PUN')
C     !BEG
      KILROY = .true.
      J      = IS-INC
C
  100 continue
        do 101 I = 1,5
          J = J+INC
          if(J.le.IE) then
            call PPUN (A(J),F(I))
          else
            if(KILROY) then
              KILROY = .false.
              F(I)   = CLOSE
            else
              F(I) = BLANK
            end if
          end if
  101   continue
        write (LU,102) F
  102   format(5A16)
      if(J.lt.IE) goto 100
C
      if(KILROY) then
        write (LU,102) CLOSE
      end if
C     !END
      call BYE ('PUN')
C
      return
      end

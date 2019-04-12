      subroutine PUD
     $(A,IS,IE,INC,WRAP,LU)
C
C     Rudolf Loeser, 2001 Jan 19
C---- Writes the values of A, for PUNT.
C     (See also PUN.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, INC, IS, J, LU
      logical KILROY, WRAP
      character BLANK*1, CLOSE*22, F*22
C     !DASH
      external  PPUD, HI, BYE
C
C               A(m,INC)
      dimension A(*)
C
      dimension F(3)
C
      data CLOSE,BLANK /' ) >                  ', ' '/
C
      call HI ('PUD')
C     !BEG
      KILROY = .true.
      J      = IS-INC
C
  100 continue
        do 101 I = 1,3
          J = J+INC
          if(J.le.IE) then
            call PPUD (A(J),F(I))
          else
            if(KILROY) then
              KILROY = .false.
              F(I)   = CLOSE
            else
              F(I) = BLANK
            end if
          end if
  101   continue
        if(KILROY) then
          write (LU,102) F,CLOSE(3:16)
  102     format(3A22,A14)
        else
          write (LU,102) F
        end if
      if(J.lt.IE) goto 100
C
      if(KILROY) then
        write (LU,102) CLOSE
      end if
C     !END
      call BYE ('PUD')
C
      return
      end

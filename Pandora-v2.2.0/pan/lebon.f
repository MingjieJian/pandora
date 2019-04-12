      subroutine LEBON
     $(A,N,NAME)
C
C     Rudolf Loeser, 2003 Aug 11
C---- Prints an array, for NOBLE.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IE, IS, LUEO, N
      character NAME*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  HI, BYE
      intrinsic min
C
C               A(N)
      dimension A(*)
C
      call HI ('LEBON')
C     !BEG
      IE = 0
  100 continue
        IS = IE+1
        IE = min((IE+8),N)
        if(IS.eq.1) then
          write (LUEO,101) NAME,(A(I),I=IS,IE)
  101     format(' ',14X,A1,1P8E14.6)
        else
          write (LUEO,102) (A(I),I=IS,IE)
  102     format(' ',15X,1P8E14.6)
        end if
      if(IE.lt.N) goto 100
C     !END
      call BYE ('LEBON')
C
      return
      end

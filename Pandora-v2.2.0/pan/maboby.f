      subroutine MABOBY
     $(FF,IMG,N,DOLOG,F)
C
C     Rudolf Loeser, 1998 Oct 16
C---- Inserts smoothed values into F, for SMOOTH.
C     !DASH
      save
C     !DASH
      real*8 AF, F, FF
      integer I, IMG, N
      logical DOLOG
C     !DASH
      external  HI, BYE
      intrinsic sign
C
C               FF(N), F(N), IMG(N)
      dimension FF(*), F(*), IMG(*)
C
      call HI ('MABOBY')
C     !BEG
      do 100 I = 1,N
        if(IMG(I).gt.0) then
          if(DOLOG) then
            AF   = exp(FF(I))
            F(I) = sign(AF,F(I))
          else
            F(I) = FF(I)
          end if
        end if
  100 continue
C     !END
      call BYE ('MABOBY')
C
      return
      end
